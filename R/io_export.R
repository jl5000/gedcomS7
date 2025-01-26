
#' Save a gedcom object to disk as a GEDCOM file
#' 
#' @details This function prepares the gedcom object and then writes it to the filepath.
#' Steps taken include filtering sensitive data, escaping "@" signs (with another "@") ,
#' and splitting long lines onto separate lines.
#'
#' @param gedcom A gedcom object.
#' @param filepath The full filepath to write to.
#' @param inc_confid Whether to include records that are marked as confidential.
#' @param inc_private Whether to include records that are marked as private.
#' @param inc_living Whether to include individual records for suspected living people.
#'
#' @returns The filepath (invisibly).
#' @export
#' @tests
#' maximal <- test_path("maximal70.ged")
#' maximal <- withr::local_tempfile(lines = fix_maximal_header(maximal), 
#'                                  fileext = ".ged")
#' ged <- read_gedcom(maximal)
#' ged@records@prefixes <- c(FAM = "F", INDI = "I", OBJE = "M", REPO = "R", 
#'                        SNOTE = "N", SOUR = "S", SUBM = "U")
#'                                
#' expect_error(write_gedcom(ged, "my_family.txt"), 
#'              regexp = "Output is not being saved as a GEDCOM file")
#' 
#' maximal <- withr::local_tempfile(fileext = ".ged")
#' roundtrip1 <- write_gedcom(ged, maximal)
#' roundtrip2 <- read_gedcom(maximal)
#' roundtrip2@records@prefixes <- c(FAM = "F", INDI = "I", OBJE = "M", REPO = "R", 
#'                        SNOTE = "N", SOUR = "S", SUBM = "U")
#' 
#' expect_identical(
#'   ged@GEDCOM,
#'   roundtrip2@GEDCOM
#' )
write_gedcom <- function(gedcom, 
                         filepath = file.choose(),
                         inc_confid = TRUE,
                         inc_private = TRUE,
                         inc_living = TRUE) {
  
  if(tolower(substr(filepath, nchar(filepath)-3 , nchar(filepath))) != ".ged")
    stop("Output is not being saved as a GEDCOM file (*.ged)")
  
  if(file.exists(filepath)) file.remove(filepath)
  
  # Write Byte Order Mark
  con <- file(filepath, encoding = "UTF-8", open = "wb")
  suppressWarnings(writeBin(as.raw(c(0xef, 0xbb, 0xbf)), con))
  close(con)
  
  con <- file(filepath, encoding = "UTF-8", open = "a")
  on.exit(close(con))
  
  if(!inc_confid) gedcom <- rm_records(gedcom, unlist(gedcom@records@XREFS_CONFID))
  if(!inc_private) gedcom <- rm_records(gedcom, unlist(gedcom@records@XREFS_PRIV))
  if(!inc_living) gedcom <- rm_living(gedcom)
  
  lines <- gedcom@GEDCOM
  
  if(!inc_confid) lines <- rm_restricted_structures(lines, "CONFIDENTIAL")
  if(!inc_private) lines <- rm_restricted_structures(lines, "PRIVACY")
  
  # Moved to GEDCOM property - in order to make the test work
  #lines2 <- prepare_gedcom_lines(lines, inc_confid, inc_private)
  
  writeLines(lines, con)
  
  invisible(filepath)
}

rm_restricted_structures <- function(lines, restriction){
  delete_ged_sections(
    lines, 
    \(x) grep(sprintf("^\\d RESN .*%s", restriction), x),
    containing_line = FALSE
  )
}


#' Remove living individuals in a GEDCOM object
#'
#' @param x A gedcom object.
#' @param max_age The maximum age to assume for a living person (if a date of birth is given).
#'
#' @returns A gedcom object cleansed of information on living individuals.
#' @export
rm_living <- function(x,
                      max_age = 100) {
  
  remove <- NULL
  for(xref in x@records@XREFS[["INDI"]]) {
    rec_lines <- x@records@RAW@INDI[[xref]]
    if(is_alive(rec_lines, max_age))
      remove <- c(remove, xref)
  }
  
  if(length(remove) > 0) x <- rm_records(x, remove)
  
  x
}

is_alive <- function(rec_lines, max_age = 100){

  deaths <- find_ged_values(rec_lines, "DEAT", return_list = TRUE)
  
  if(length(deaths) > 0){
    # death events exist - Y/date/place/addr
    death_occured <- c(
      grepl("^1 DEAT Y$", unlist(deaths)),
      grepl("^2 (DATE|PLAC|ADDR) ", unlist(deaths))
    )
    if(sum(death_occured) > 0) return(FALSE)
  }
  
  dobs <- find_ged_values(rec_lines, c("BIRT","DATE"))
  dobs <- dobs[dobs != ""]
  
  if(length(dobs) > 0){
    ages <- unlist(lapply(dobs, date_diff, minimise = TRUE))
    if(suppressWarnings(max(ages, na.rm = TRUE)) > max_age) return(FALSE)
  }
  
  TRUE
}


#' Determine the number of years between two dates
#' 
#' @param date1 A GEDCOM date string.
#' @param date2 A GEDCOM date string. If no date is given, today's date is used.
#' @param minimise If date ranges or periods are used in the dates, whether to choose the bounds which
#' assume the minimum date difference. If this is FALSE, the maximum date difference is assumed.
#'
#' @returns A numeric value giving the number of years. A numeric value less than zero means no
#' determination could be made.
#' @keywords internal
#' @tests
#' expect_equal(date_diff("1900", "2000"), 99, tolerance = 0.01)
#' expect_equal(date_diff("1900", "2000", minimise = FALSE), 101, tolerance = 0.01)
#' expect_equal(date_diff("800", "2020"), 1219, tolerance = 0.01)
#' expect_equal(date_diff("28 JAN 2006", "14 DEC 2008"), 2.877, tolerance = 0.01)
#' expect_equal(date_diff("BET JAN 2000 AND 2007", "FROM 2012 TO 8 MAY 2016"), 4, tolerance = 0.01)
#' expect_equal(date_diff("BET JAN 2000 AND 2007", "FROM 2012 TO 8 MAY 2016", minimise = FALSE), 16.35, tolerance = 0.01)
#' expect_equal(date_diff("ABT 1932", "CAL 2000"), 67, tolerance = 0.01)
date_diff <- function(date1,
                      date2 = NULL,
                      minimise = TRUE) {
  
  date2 <- date2 %||% date_exact_current()@GEDCOM_STRING
  
  date1 <- parse_gedcom_date(date1, minimise = !minimise)
  date2 <- parse_gedcom_date(date2, minimise = minimise)
  
  if(is.na(date1) || is.na(date2)) return(-1)
  
  as.numeric(difftime(date2, date1, units = "days")) / 365.25
}

#' Prepare GEDCOM lines for export
#'
#' @param lines A character vector of gedcom lines.
#'
#' @returns A vector of GEDCOM lines ready for export.
#' @keywords internal
prepare_gedcom_lines <- function(lines){
  
  check_for_xref_mentions(lines)
    
  split_gedcom_values(lines) |> 
    add_at_escapes()
}

#' Check gedcom lines for inappropriate mentions of xrefs
#'
#' @param lines A character vector of gedcom lines.
#'
#' @returns Nothing. If applicable, the function will result in a warning if
#' inappropriate mentions of xrefs are found.
#' @keywords internal
check_for_xref_mentions <- function(lines){
  
  # Check for xrefs mentioned beyond pointers
  vals <- parse_line_value(lines)
  line_no <- intersect(
    grep(reg_xref(FALSE), vals), # xrefs appear
    grep(reg_xref(TRUE), vals, invert = TRUE) # but they do not appear alone
  )
  
  if(length(line_no) > 0){
    warning("The following line numbers mention cross-reference identifiers - it is not recommended you refer to these directly as they can change between systems: ", 
            toString(line_no))
  }
}


add_at_escapes <- function(lines){
  
  # For lines with these patterns, we don't want to replace @ with @@
  exclude <- unique(c(
    grep(".+", parse_line_xref(lines)), # start of record
    grep(reg_xref(TRUE), parse_line_value(lines)) # xref pointer
  ))
  
  # sub() will pick up the first instance which should be the beginning of the value
  lines[-exclude] <- sub(" @", " @@", lines[-exclude])
  
  lines
  
}

#' Create continuation lines
#' 
#' This function introduces CONT lines for line values that contain line breaks.
#'
#' @param lines A character vector of gedcom lines.
#' 
#' @returns A new character vector of gedcom lines, possibly expanded to include CONT lines.
#' @keywords internal
#' @tests
#' test1 <- c(
#' "0 TEMP",
#' "1 TAG This is a line\nthen this\nand this\nalso this"
#' )
#' test2 <- c(
#'   "0 TEMP",
#'   "1 TAG A tag",
#'   "2 QUAY This is a line\nthen this\nand this\nalso this",
#'   "1 DATE Today"
#' )
#' 
#' expect_snapshot_value(split_gedcom_values(test1), "json2")
#' expect_snapshot_value(split_gedcom_values(test2), "json2")
split_gedcom_values <- function(lines) {

  lines <- gsub("\n\r|\r\n", "\n", lines)
  lines <- gsub("\r", "\n", lines)
  lines <- gsub("\n", "\n0 CONT ", lines)
  lines <- unlist(strsplit(lines, "\n"))
  
  lvl <- 0
  for(i in seq_along(lines)){
    if(substr(lines[i], 1, 6) == "0 CONT"){
      lines[i] <- increase_level(lines[i], lvl + 1)
    } else {
      lvl <- parse_line_level(lines[i])
    }
  }
  
  lines
}


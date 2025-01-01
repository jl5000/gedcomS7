
#' Import a GEDCOM file
#'
#' Imports a *.ged file and creates a gedcom object.
#'
#' @param filepath The full filepath of the GEDCOM file.
#'
#' @returns A gedcom S7 object.
#' @export
#' @tests
#' maximal <- test_path("maximal70.ged")
#' maximal <- withr::local_tempfile(lines = fix_maximal_header(maximal), 
#'                                  fileext = ".ged")
#' length_maximal <- length(readLines(maximal))
#' ged <- read_gedcom(maximal)
#' expect_equal(length(ged@c_as_ged), length_maximal)
read_gedcom <- function(filepath = file.choose()) {
  
  if(tolower(substr(filepath, nchar(filepath)-3 , nchar(filepath))) != ".ged")
    stop("GEDCOM file should have a .ged extension")
  
  con <- file(filepath, encoding = "UTF-8")
  on.exit(close(con))
  
  ged_lines_raw <- readLines(con)
  
  ged_lines <- ged_lines_raw |> 
    validate_lines() |> 
    remove_at_escapes() |> 
    combine_cont_lines()
  
  records_lst <- split(ged_lines, cumsum(substr(ged_lines, 1, 1) == "0"))
  
  ged <- parse_records(records_lst)
  
  check_unparsed(ged_lines_raw, ged)
  
  ged
}


validate_lines <- function(lines){
  
  invalid_lines <- grep(reg_ged_line(), lines, invert = TRUE)
  
  if(length(invalid_lines) > 0)
    stop(paste(c("The following lines are invalid:", 
                 paste(invalid_lines, lines[invalid_lines], sep = ": ")), 
               collapse = "\n"))
  
  #check characters in components
  
  #non-greg calendars not allowed
  date_lines <- parse_line_tag(lines) == "DATE"
  non_greg_lines <- grepl("(JULIAN|FRENCH_R|HEBREW)", parse_line_value(lines))
  unsupp_date_rows <- which(date_lines & non_greg_lines)
  
  if(length(unsupp_date_rows) > 0)
    stop("Non-Gregorian dates are not supported. See line ", 
         toString(unsupp_date_rows))
  
  # Make explicit GREGORIANs implicit
  lines[date_lines] <- gsub("GREGORIAN ", "", lines[date_lines])
  
  if(lines[1] != "0 HEAD")
    stop("The file does not start with a HEAD record")
  
  if(rev(lines)[1] != "0 TRLR")
    stop("The file does not end with a TRLR record")
  
  # UIDs should only appear once
  uid_rows <- grep("^\\d UID ", lines)
  if(length(uid_rows) > 0){
    vals <- parse_line_value(lines)
    uids <- vals[uid_rows]
    dupes <- duplicated(uids)
    if(sum(dupes) > 0)
      stop("Some UIDs are duplicated: ", toString(unique(uids[dupes])))
  }
  
  lines
}

remove_at_escapes <- function(lines){
  
  vals <- parse_line_value(lines)
  
  escape_lines <- grep("^@@", vals)
  
  if(length(escape_lines) == 0) return(lines)
  
  lines[escape_lines] <- sub("^([0-9]+ [A-Z0-9_]+ )@", "\\1", lines[escape_lines])
  lines
}

#' Combine continuation lines into the parent line
#' 
#' @param lines A character vector of gedcom lines.
#'
#' @returns A new character vector of gedcom lines.
#' @keywords internal
combine_cont_lines <- function(lines) {
  
  reg_cont <- sprintf("^\\d+ CONT (.*)$")
  cont_lines <- grep(reg_cont, lines)
  if(length(cont_lines) == 0) return(lines)
  
  unique_delim <- "<><>unique_delim<><>"
  
  lines <- sub("^(\\d+ CONT )", "\\1\n", lines)
  
  # Prepare lines for merging
  lines[cont_lines] <- sub(reg_cont, "\\1", lines[cont_lines])
  lines[-cont_lines] <- paste0(unique_delim, lines[-cont_lines])
  
  # Merge and separate again
  lines <- paste(lines, collapse = "") |>
    strsplit(unique_delim) |>
    unlist()
  
  lines[-1] # delete empty line introduced
}


parse_records <- function(records_lst){
  
  # Remove TRLR
  records_lst <- records_lst[-length(records_lst)]
  
  # parse header
  x <- parse_gedcom_header(records_lst[[1]])
  records_lst <- records_lst[-1]
  
  # Lambda fn to get a list of records of a particular type
  subset_recs <- \(rec_lst, rec_type){
    recs <- Filter(\(x) grepl(sprintf("^0.* %s", rec_type), x[1]), rec_lst)
    names(recs) <- vapply(recs, \(rec) parse_line_xref(rec[1]), FUN.VALUE = character(1))
    recs
  }
  
  S7::props(x) <- list(
    indi = subset_recs(records_lst, "INDI"),
    fam = subset_recs(records_lst, "FAM"),
    sour = subset_recs(records_lst, "SOUR"),
    repo = subset_recs(records_lst, "REPO"),
    media = subset_recs(records_lst, "OBJE"),
    note = subset_recs(records_lst, "SNOTE"),
    subm = subset_recs(records_lst, "SUBM")
  )
  
  x <- add_missing_xrefs(x)
  
  x
}

add_missing_xrefs <- function(gedcom){
  
  for(rec_type in names(gedcom@c_xrefs)){
    for(rec_no in seq_along(S7::prop(gedcom, rec_type))){
      if(names(S7::prop(gedcom, rec_type))[[rec_no]] != "") next
      
      new_xref <- gedcom@c_next_xref[rec_type]
      first_line <- S7::prop(gedcom, rec_type)[[rec_no]][1]
      S7::prop(gedcom, rec_type)[[rec_no]][1] <- sub("^0", paste(0, new_xref), first_line)
      names(S7::prop(gedcom, rec_type))[rec_no] <- new_xref
    }
  }
  
  gedcom
}

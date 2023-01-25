
#' Import a GEDCOM file
#'
#' Imports a *.ged file and creates a tidyged object.
#'
#' @param filepath The full filepath of the GEDCOM file.
#'
#' @return A tidyged object
#' @export
#'
#' @examples
#' \dontrun{
#' read_gedcom("C:/my_family.ged")
#' }
#' @tests
#' expect_error(read_gedcom("my_family.txt"))
#' expect_snapshot_value(
#'     read_gedcom(system.file("extdata", "555SAMPLE.GED", package = "tidyged.io")), 
#'     "json2")
#' expect_snapshot_value(
#'     read_gedcom(system.file("extdata", "555SAMPLE16BE.GED", package = "tidyged.io")), 
#'     "json2")
#' expect_snapshot_value(
#'     read_gedcom(system.file("extdata", "555SAMPLE16LE.GED", package = "tidyged.io")), 
#'     "json2")
#' expect_snapshot_value(
#'     read_gedcom(system.file("extdata", "MINIMAL555.GED", package = "tidyged.io")), 
#'     "json2")
read_gedcom <- function(filepath = file.choose()) {
  
  if(tolower(substr(filepath, nchar(filepath)-3 , nchar(filepath))) != ".ged")
    stop("GEDCOM file should have a .ged extension")
  
  gedcom_encoding <- read_gedcom_encoding(filepath)
  
  con <- file(filepath, encoding = gedcom_encoding)
  on.exit(close(con))
  
  ged_lines <- readLines(con)
  
  validate_lines(ged_lines)
  
  validate_header(ged_lines[1:6], gedcom_encoding)
  
  ged_lines <- combine_gedcom_values(ged_lines) |>
    gsub(pattern = "@@", replacement = "@")
  
  records_lst <- split(ged_lines, cumsum(substr(ged_lines, 1, 1) == "0"))
  
  x <- parse_records(records_lst)
  
  x@file_path <- filepath
  message("If you would like to enable quicksave set @quicksave = TRUE. This will allow you to use @save to save your changes to the same file. Exercise care with this option as it will not ask for confirmation before overwriting changes.")
  
  x
}


#' Read the Byte Order Mark of the GEDCOM file
#' 
#' This function reads the Byte Order Mark of a GEDCOM file in order to determine its encoding.
#' It only checks for UTF-8 or UTF-16 - if neither of these are found it throws an error.
#'
#' @param filepath The full filepath of the GEDCOM file.
#'
#' @return A character string indicating the encoding of the file.
#' @tests
#' expect_equal(
#'   read_gedcom_encoding(system.file("extdata", "555SAMPLE.GED", package = "tidyged.io")), 
#'   "UTF-8")
#' expect_equal(
#'   read_gedcom_encoding(system.file("extdata", "555SAMPLE16BE.GED", package = "tidyged.io")), 
#'   "UTF-16BE")
#' expect_equal(
#'   read_gedcom_encoding(system.file("extdata", "555SAMPLE16LE.GED", package = "tidyged.io")), 
#'   "UTF-16LE")
read_gedcom_encoding <- function(filepath) {
  
  if(identical(as.character(readBin(filepath, 'raw', 3)), .pkgenv$BOM_UTF8)) {
    return("UTF-8")  
  } else if(identical(as.character(readBin(filepath, 'raw', 2)), .pkgenv$BOM_UTF16_BE)) {
    return("UTF-16BE")
  } else if(identical(as.character(readBin(filepath, 'raw', 2)), .pkgenv$BOM_UTF16_LE)) {
    return("UTF-16LE")
  } else {
    stop("Invalid file encoding. Only UTF-8 and UTF-16 Byte Order Marks are supported")
  }
  
}

validate_lines <- function(lines){
  
  if(any(nchar(lines) > .pkgenv$gedcom_line_length_limit)) 
    stop("This is not a GEDCOM 5.5.5 file. The following lines are too long: ", 
         paste(which(nchar(lines) > .pkgenv$gedcom_line_length_limit), collapse=","))
  
  line_vals <- extract_ged_value(lines)
  
  if(any(nchar(line_vals) > .pkgenv$gedcom_phys_value_limit)) 
    stop("This is not a GEDCOM 5.5.5 file. The following lines have values which are too long: ", 
         paste(which(nchar(line_vals) > .pkgenv$gedcom_phys_value_limit), collapse=","))

  invalid_lines <- grep(reg_ged_line(), lines, invert = TRUE)
  
  if(length(invalid_lines) > 0)
    stop(paste(c("The following lines are invalid:", 
                 paste(invalid_lines, lines[invalid_lines], sep = ": ")), 
               collapse = "\n"))
  
    unsupp_calendars <- c("HEBREW","FRENCH R","JULIAN","UNKNOWN")
  unsupp_calendars <- paste0("@#D", unsupp_calendars, "@", collapse = "|")
  
  if(any(grepl(sprintf("^[1-6] DATE (%s)", unsupp_calendars), lines)))
    stop("Non-Gregorian calendar dates are not supported.")
  
  NULL
}

validate_header <- function(header_lines, expected_encoding) {
  
  expected_header = c(
    "0 HEAD",
    "1 GEDC",
    "2 VERS 5.5.5",
    "2 FORM LINEAGE-LINKED",
    "3 VERS 5.5.5"
  )
  
  if(!isTRUE(all.equal(header_lines[1:5], expected_header)))
    stop("Malformed header")
  
  char <- sub("1 CHAR ", "", header_lines[6])
  
  if(expected_encoding == "UTF-8") {
    if(char != "UTF-8") stop("Character encodings do not match")
  } else if(expected_encoding %in% c("UTF-16BE", "UTF-16LE")) {
    if(char != "UNICODE") stop("Character encodings do not match")
  } else {
    stop("Character encoding not recognised")
  }
  
  NULL
}

#' Convert the GEDCOM grammar to the GEDCOM form
#' 
#' This function applies concatenation indicated by CONC/CONT lines.
#' 
#' The function works by collapsing CONC/CONT lines using group-by/summarise.   
#'
#' @param gedcom A tidyged object.
#'
#' @return A tidyged object in the GEDCOM form.
combine_gedcom_values <- function(lines) {
  
  reg_cont_conc <- sprintf("^[1-6] (CONT|CONC) (.*)$")
  cont_conc_lines <- grep(reg_cont_conc, lines)
  if(length(cont_conc_lines) == 0) return(lines)
  
  unique_delim <- "<><>unique_delim<><>"
  
  lines <- lines |> 
    gsub(pattern = "\n\r|\r\n", replacement = "\n") |> 
    gsub(pattern = "\r", replacement = "\n") |>
    sub(pattern = "^([1-6] CONT )", replacement = "\\1\n")
  
  # Prepare lines for merging
  lines[cont_conc_lines] <- sub(reg_cont_conc, "\\2",
                                lines[cont_conc_lines])
  lines[-cont_conc_lines] <- paste0(unique_delim,
                                    lines[-cont_conc_lines])
  
  # Merge and seperate again
  lines <- paste(lines, collapse = "") |>
    strsplit(unique_delim) |>
    unlist()
  
  lines[-1] # delete empty line introduced
}


parse_records <- function(records_lst){
  
  if(!grepl(sprintf("^0 %s SUBM$", reg_xref(FALSE)), records_lst[[2]][1]))
    stop("The record immediately after the header record must be the Submitter record")
  
  if(records_lst[[length(records_lst)]][1] != "0 TRLR")
    stop("The file does not end with a TRLR record")
  
  records_lst <- records_lst[-length(records_lst)]
  
  # parse subm and header
  x <- create_gedcom(records_lst)
  records_lst <- records_lst[-(1:2)]
  
  subset_recs <- function(rec_lst, pattern){
    recs <- Filter(\(x) grepl(sprintf(pattern, reg_xref(FALSE)), x[1]), rec_lst)
    names(recs) <- sapply(recs, \(x) regmatches(x[1], regexpr(reg_xref(FALSE), x[1])))
    recs
  }

  R7::props(x) <- list(
    indi = subset_recs(records_lst, "^0 %s INDI$"),
    famg = subset_recs(records_lst, "^0 %s FAM$"),
    sour = subset_recs(records_lst, "^0 %s SOUR$"),
    repo = subset_recs(records_lst, "^0 %s REPO$"),
    media = subset_recs(records_lst, "^0 %s OBJE$"),
    note = subset_recs(records_lst, "^0 %s NOTE .+$")
  )

  x
}


create_gedcom <- function(records_lst){
  
  subm_lines <- records_lst[[2]]

  subm_nts <- find_ged_values(subm_lines, "NOTE")
  
  subm <- class_record_subm(
    xref = extract_ged_xref(subm_lines[1]),
    name = find_ged_values(subm_lines, "NAME"),
    address = extract_address(subm_lines),
    media_links = find_ged_values(subm_lines, "OBJE"),
    auto_id = find_ged_values(subm_lines, "RIN"),
    note_links = subm_nts[grepl(reg_xref(TRUE), subm_nts)],
    notes = subm_nts[!grepl(reg_xref(TRUE), subm_nts)],
    last_updated = extract_change_date(subm_lines)
  )
  
  hd_lines <- records_lst[[1]]
  
  class_gedcomR7(
    system_id = find_ged_values(hd_lines, c("HEAD","SOUR")),
    product_name = find_ged_values(hd_lines, c("HEAD","SOUR","NAME")),
    product_version = find_ged_values(hd_lines, c("HEAD","SOUR","VERS")),
    business_name = find_ged_values(hd_lines, c("HEAD","SOUR","CORP")),
    business_address = extract_address(hd_lines, c("HEAD","SOUR","CORP")),
    source_data_name = find_ged_values(hd_lines, c("HEAD","SOUR","DATA")),
    source_data_pubdate = find_ged_values(hd_lines, c("HEAD","SOUR","DATA","DATE")),
    source_data_copyright = find_ged_values(hd_lines, c("HEAD","SOUR","DATA","COPR")),
    receiving_system = find_ged_values(hd_lines, c("HEAD","DEST")),
    creation_date = find_ged_values(hd_lines, c("HEAD","DATE")),
    creation_time = find_ged_values(hd_lines, c("HEAD","DATE","TIME")),
    language = find_ged_values(hd_lines, c("HEAD","LANG")),
    xref_subm = find_ged_values(hd_lines, c("HEAD","SUBM")),
    file_name = find_ged_values(hd_lines, c("HEAD","FILE")),
    gedcom_copyright = find_ged_values(hd_lines, c("HEAD","COPR")),
    content_description = find_ged_values(hd_lines, c("HEAD","NOTE")),
    subm = subm
  )
  
}


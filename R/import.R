
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
  
  if(tolower(stringr::str_sub(filepath, -4, -1)) != ".ged")
    stop("GEDCOM file should have a .ged extension")
  
  gedcom_encoding <- read_gedcom_encoding(filepath)
  
  con <- file(filepath, encoding = gedcom_encoding)
  on.exit(close(con))
  
  ged_lines <- readLines(con) |> 
    stringr::str_trim(side = "left") |> 
    check_line_lengths(.pkgenv$gedcom_line_length_limit)
  
  ged <- tibble::tibble(value = ged_lines) |>
    tidyr::extract(value, into = c("level", "record", "tag", "value"), 
                   regex = "^(\\d) (@.+@)? ?(\\w{3,5}) ?(.*)$") |>
    dplyr::mutate(record = dplyr::if_else(tag == "HEAD", "HD", record),
                  record = dplyr::if_else(tag == "TRLR", "TR", record),
                  record = dplyr::na_if(record, "")) |>
    tidyr::fill(record) |> 
    dplyr::mutate(level = as.numeric(level),
                  value = stringr::str_replace_all(value, "@@", "@")) |> 
    combine_gedcom_values() |> 
    combine_spouse_age_lines() |> 
    capitalise_tags_and_keywords() |> 
    tidyged.internals::set_class_to_tidyged()
  
  validate_gedcom(ged, gedcom_encoding)
  ged
  
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


#' Check the line lengths of a GEDCOM file
#' 
#' @param lines A character vector of GEDCOM lines.
#' @param limit The maximum length of a line allowed.
#'
#' @return The input character vector
#' @tests
#' expect_error(check_line_lengths(c("the", "quick", "brown", "fox"), 4))
#' expect_equal(check_line_lengths(c("the", "quick", "brown", "fox"), 5),
#'                                 c("the", "quick", "brown", "fox"))
check_line_lengths <- function(lines, limit) {
  
  if(any(nchar(lines) > limit)) 
    stop("This is not a GEDCOM 5.5.5 file. The following lines are too long: ", 
         paste(which(nchar(lines) > limit), collapse=","))
  
  lines
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
combine_gedcom_values <- function(gedcom) {
  
  tags <- c("CONT", "CONC")
  
  gedcom |> 
    dplyr::mutate(value = stringr::str_replace_all(value, "\n\r|\r\n", "\n"),
                  value = stringr::str_replace_all(value, "\r", "\n")) |>
    dplyr::mutate(row = dplyr::row_number()) |> 
    dplyr::mutate(value = dplyr::if_else(tag == "CONT", paste0("\n", value), value),
                  row = dplyr::if_else(tag %in% tags, NA_integer_, row),
                  tag = dplyr::if_else(tag %in% tags, NA_character_, tag)) |>
    tidyr::fill(tag, row, .direction = "down") |>
    dplyr::group_by(record, tag, row) |> 
    dplyr::summarise(level = min(level),
                     value = paste(value, collapse = ""),
                     .groups = "drop") |>
    dplyr::ungroup() |> 
    dplyr::arrange(row) |>
    dplyr::select(level, record, tag, value)
  
}

#' Combine husband/wife age rows into one row
#' 
#' This function combines the HUSB and WIFE rows with their subordinate AGE rows in order
#' to make querying easier.
#' 
#' @param gedcom A tidyged object.
#'
#' @return A tidyged object with husband/wife and age rows combined accordingly.
combine_spouse_age_lines <- function(gedcom){
  
  spouse_rows <- which(gedcom$tag %in% c("HUSB","WIFE") & gedcom$value == "")
  
  if(length(spouse_rows) == 0) return(gedcom)
  
  gedcom$value[spouse_rows] <- gedcom$value[spouse_rows + 1]
  gedcom$tag[spouse_rows] <- paste0(gedcom$tag[spouse_rows], "_AGE")
  gedcom <- gedcom[-(spouse_rows+1),]
  gedcom
}


#' Capitalise tags and certain keywords
#' 
#' This function capitalises all tags and certain values such as SEX values and DATE values.
#' 
#' @details The function also ensures certain values are lowercase such as PEDI
#' and ADOP values, and removes explicit GREGORIAN date escape sequences (as they are implied).
#'
#' @param gedcom A tidyged object.
#'
#' @return A tidyged object with appropriately capitalised tags and keywords.
capitalise_tags_and_keywords <- function(gedcom){
  
  gedcom |> 
    dplyr::mutate(tag = toupper(tag)) |> 
    dplyr::mutate(value = dplyr::if_else(tag == "SEX", toupper(value), value),
                  value = dplyr::if_else(tag == "PEDI", tolower(value), value),
                  value = dplyr::if_else(tag == "ADOP", toupper(value), value),
                  value = dplyr::if_else(level == 2 & tag == "EVEN", toupper(value), value),
                  value = dplyr::if_else(tag == "LATI", toupper(value), value),
                  value = dplyr::if_else(tag == "LONG", toupper(value), value),
                  value = dplyr::if_else(tag == "ROLE" & 
                                           !stringr::str_detect(value, tidyged.internals::reg_custom_value()), 
                                         toupper(value), value),
                  value = dplyr::if_else(tag == "DATE" &
                                           !stringr::str_detect(value, tidyged.internals::reg_custom_value()), 
                                         toupper(value), value),
                  value = dplyr::if_else(tag == "DATE" &
                                           !stringr::str_detect(value, tidyged.internals::reg_custom_value()),
                                         stringr::str_remove(value, "@#DGREGORIAN@ "), value)) |> 
    dplyr::mutate(tag = dplyr::if_else(tag == "URL", "WWW", tag))
  
  
}

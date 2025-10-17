
parse_line_level <- function(lines){
  sub(reg_ged_line(), "\\1", lines) |> as.integer()
}
parse_line_xref <- function(lines){
  sub(reg_ged_line(), "\\2", lines)
}
parse_line_tag <- function(lines){
  sub(reg_ged_line(), "\\3", lines)
}
parse_line_value <- function(lines){
  sub(reg_ged_line(), "\\4", lines)
}

parse_vals_and_types <- function(lines, val_tag){
  val_lst <- find_ged_values(lines, val_tag, return_list = TRUE)
  if(length(val_lst) == 0) return(character())
  
  vals <- vapply(val_lst, \(x) parse_line_value(x[1]), FUN.VALUE = character(1))
  types <- vapply(val_lst, \(x) {
    if(length(x) == 1) return("")
    parse_line_value(x[2])
  }, FUN.VALUE = character(1))
  names(vals) <- types
  vals
}

#' Convert a GEDCOM date into a date object
#'
#' @param date_string A GEDCOM date value string.
#' @param minimise Whether to fill in missing date pieces so that the date is minimised. 
#' For example, if no month is given, January is used. If minimise = FALSE, December will be used.
#'
#' @returns A date.
#' @export
#' @tests
#' expect_equal(parse_gedcom_date("BEF 1980"), as.Date(NA_character_))
#' expect_equal(parse_gedcom_date("TO 23 JUN 2001"), as.Date(NA_character_))
#' expect_equal(parse_gedcom_date("AFT 1600", FALSE), as.Date(NA_character_))
#' expect_equal(parse_gedcom_date("FROM FEB 1900", FALSE), as.Date(NA_character_))
#' expect_equal(parse_gedcom_date("34 JAN 2000"), as.Date(NA_character_))
#' expect_equal(parse_gedcom_date("2005"), as.Date("2005-01-01"))
#' expect_equal(parse_gedcom_date("2005", FALSE), as.Date("2005-12-31"))
#' expect_equal(parse_gedcom_date("JUL 1989"), as.Date("1989-07-01"))
#' expect_equal(parse_gedcom_date("JUL 1989", FALSE), as.Date("1989-07-31"))
#' expect_equal(parse_gedcom_date("25 MAR 1980"), as.Date("1980-03-25"))
#' expect_equal(parse_gedcom_date("25 MAR 1980", FALSE), as.Date("1980-03-25"))
#' expect_equal(parse_gedcom_date("FROM 25 MAR 1980 TO 1990", TRUE), as.Date("1980-03-25"))
#' expect_equal(parse_gedcom_date("FROM 25 MAR 1980 TO 1990", FALSE), as.Date("1990-12-31"))
parse_gedcom_date <- function(date_string, minimise = TRUE){
  
  stopifnot(
    "date_string must be a character string" = is.character(date_string) &&
      length(date_string) == 1,
    "minimise must be a boolean" = is.logical(minimise) &&
      length(minimise) == 1
  )
  
  # Extract relevant date_calendar
  if(minimise){
    if(grepl("^(BEF|TO) ", date_string)) return(as.Date(NA_character_))
    
    date_string <- sub(sprintf("^[A-Z ]*?(%s).*?$", reg_date_calendar(only = FALSE,
                                                                       strict = FALSE)), 
                       "\\1", date_string)
  } else {
    if(grepl("^(AFT|FROM) ", date_string) &&
       !grepl(" TO ", date_string)) return(as.Date(NA_character_))
    
    date_string <- sub(sprintf("^.*?(%s).*?$", reg_date_calendar(only = FALSE,
                                                                  strict = FALSE)), 
                       "\\1", date_string)
  }
  
  date_string <- gsub("(GREGORIAN|JULIAN) ", "", date_string)
  ged_year <- sub(".* ", "", date_string)
  
  if(grepl("[A-Z]{3}", date_string)) {
    ged_month <- which(toupper(month.abb) == gsub("[^A-Z]", "", date_string))
  } else {
    if(minimise) ged_month <- 1 else ged_month <- 12
  }
  
  if(grepl("^\\d{1,2} ", date_string)) {
    ged_day <- gsub(" .*", "", date_string)
  } else {
    if(minimise) {
      ged_day <- 1
    } else {
      ged_day <- paste(10, ged_month, ged_year) |> 
        as.Date(format = "%d %m %Y") |> 
        days_in_month()
    }
  }
  
  paste(ged_day, ged_month, ged_year) |> 
    as.Date(format = "%d %m %Y")
}

days_in_month <- function(date) {
  m <- format(date, format="%m")
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  as.integer(format(date - 1, format="%d"))
}

#' Convert a GEDCOM age at event into decimalised years
#'
#' @param age_string A string describing an age at an event, 
#' e.g. "14y 3m 1w 2d".
#'
#' @returns A numeric value giving the age in years.
#' @export
#' @tests
#' expect_equal(parse_gedcom_age("16y"), 16)
#' expect_equal(parse_gedcom_age("16y 6m"), 16.5)
#' expect_equal(parse_gedcom_age("73d"), 73/365)
#' expect_equal(parse_gedcom_age("12w"), 12/52)
#' expect_equal(parse_gedcom_age("3y 2m 1w 5d"), 3+(2/12)+(1/52)+(5/365))
parse_gedcom_age <- function(age_string) {
  
  stopifnot(
    "age_string must be a character string" = is.character(age_string) &&
      length(age_string) == 1
  )
  
  years <- sub(".*?(\\d{1,3})y.*", "\\1", age_string)
  months <- sub(".*?(\\d{1,2})m.*", "\\1", age_string)
  weeks <- sub(".*?(\\d{1,2})w.*", "\\1", age_string)
  days <- sub(".*?(\\d{1,3})d.*", "\\1", age_string)
  
  if(age_string == years) years_num <- 0 else years_num <- as.numeric(years)
  if(age_string == months) months_prop <- 0 else months_prop <- as.numeric(months)/12
  if(age_string == weeks) weeks_prop <- 0 else weeks_prop <- as.numeric(weeks)/52
  if(age_string == days) days_prop <- 0 else days_prop <- as.numeric(days)/365
  
  years_num + months_prop + weeks_prop + days_prop
  
}

check_unparsed <- function(lines, parsed){
  
  not_parsed <- setdiff(
    lines,
    parsed@GEDCOM
  )
  
  # Exception - remove instances where a missing xref would have been added,
  # meaning the lines won't be identical anyway
  not_parsed <- grep("^0 [A-Z]+$", not_parsed, value = TRUE, invert = TRUE)
  
  if(length(not_parsed) > 0)
    warning("The following lines could not be parsed:\n", 
            paste(not_parsed, collapse = "\n"))
  
}

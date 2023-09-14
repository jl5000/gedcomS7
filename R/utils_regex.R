

group_it <- function(reg) {
  sprintf("(?:%s)", reg)
}


anchor_it <- function(reg) {
  sprintf("^%s$", reg)
}


reg_tag <- function(std = TRUE){
  if(std){
    "[A-Z][A-Z0-9_]*"
  } else {
    "_[A-Z0-9_]+"
  }
}

reg_ged_line <- function(){
  # \\1 is level, \\2 is xref, \\3 is tag, \\4 is value
  sprintf("^([0-9]+)(?: (%s))? (%s|%s)(?: (.*))?$", 
          reg_xref(FALSE), 
          reg_tag(TRUE), 
          reg_tag(FALSE))
}

#' Enumerate all combinations of regex patterns
#'
#' @param reg1 A vector of regex patterns.
#' @param reg2 A vector of regex patterns.
#'
#' @return A vector of all combinations of the concatenation of reg1 and reg2.
regex_combn <- function(reg1, reg2) {
  paste(rep(reg1, each = length(reg2)), reg2, sep = "")
}

reg_day <- function() {
  paste(1:31, collapse = "|") |> group_it()
}

reg_month <- function() {
  paste0(toupper(month.abb), collapse = "|") |> group_it()
}

reg_year <- function() {
  "\\d{1,4}" |> group_it()
}

#' @tests
#' expect_equal(grepl(reg_time(TRUE), "0:00"), TRUE)
#' expect_equal(grepl(reg_time(TRUE), "23:59"), TRUE)
#' expect_equal(grepl(reg_time(TRUE), "0:00:00"), TRUE)
#' expect_equal(grepl(reg_time(TRUE), "23:59:59"), TRUE)
#' expect_equal(grepl(reg_time(TRUE), "0:00:00.000"), TRUE)
#' expect_equal(grepl(reg_time(TRUE), "23:59:59.9999"), TRUE)
#' expect_equal(grepl(reg_time(TRUE), "00:00"), TRUE)
#' expect_equal(grepl(reg_time(TRUE), "24:59"), FALSE)
#' expect_equal(grepl(reg_time(TRUE), "23:60"), FALSE)
#' expect_equal(grepl(reg_time(TRUE), "23:59:60"), FALSE)
#' expect_equal(grepl(reg_time(TRUE), "23:59:59."), FALSE)
#' expect_equal(grepl(reg_time(TRUE), "23:59:59:a"), FALSE)
reg_time <- function(only = TRUE){
  hh <- "(\\d)|((0|1)\\d)|(2(0|1|2|3))"
  mm <- "(0|1|2|3|4|5)\\d"
  ss <- mm
  fs <- "\\d+"
  reg <- sprintf("(%s):(%s)(:(%s)(\\.(%s))?)?Z?", hh, mm, ss, fs)
  if(only) reg <- anchor_it(reg)
  reg
}

#' Construct a regular expression for an xref
#'
#' @param only Whether to allow strings of only xrefs. If FALSE,
#' the regular expression accepts patterns where text can come before or after
#' the xref.
#'
#' @return A regular expression pattern for an xref.
#' @export
reg_xref <- function(only = TRUE) {
  reg <- "@[A-Z0-9_]+@"
  if(only) reg <- anchor_it(reg)
  reg
}

reg_uuid <- function(only = TRUE){
  reg <- "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
  if(only) reg <- anchor_it(reg)
  reg
}

reg_latitude <- function() {
  "[NS]\\d{1,2}(\\.\\d{1,6})?"
}

reg_longitude <- function() {
  "[EW]\\d{1,3}(\\.\\d{2,6})?"
}

reg_age_at_event <- function() {
  y <- "\\d{1,3}y"
  m <- "\\d{1,2}m"
  w <- "\\d{1,2}w"
  d <- "\\d{1,3}d"
  
  paste0("^(?:[<>] )?",
         c(sprintf("%s( %s)?( %s)?( %s)?$", y, m, w, d),
           sprintf("%s( %s)?( %s)?$", m, w, d),
           sprintf("%s( %s)?$", w, d),
           sprintf("%s$", d))) |> 
    paste(collapse = "|")
  
}

reg_role_in_event <- function(){
  paste(anchor_it(val_roles()) , collapse = "|")
}


#' Construct a regular expression for DATE_EXACT values
#' 
#' @param only Whether to allow strings of only date_exact. If FALSE,
#' the regular expression accepts patterns where text can come before or after
#' the date_exact().
#' @return A regex string
#' @tests
#' expect_equal(grepl(reg_date_exact(), "14 JAN 2005"), TRUE)
#' expect_equal(grepl(reg_date_exact(), "14 JAM 2005"), FALSE)
#' expect_equal(grepl(reg_date_exact(), "JAN 2005"), FALSE)
#' expect_equal(grepl(reg_date_exact(), "14 JAN 2005/06"), FALSE)
#' expect_equal(grepl(reg_date_exact(), "5 JUL 2005"), TRUE)
#' expect_equal(grepl(reg_date_exact(), "8 NOV 1956/57"), FALSE)
#' expect_equal(grepl(reg_date_exact(), "2005"), FALSE)
#' expect_equal(grepl(reg_date_exact(), "15 NOV 125"), TRUE)
#' expect_equal(grepl(reg_date_exact(), "JAN 1901/58"), FALSE)
#' expect_equal(grepl(reg_date_exact(), "5 JUL 2005 "), FALSE)
#' expect_equal(grepl(reg_date_exact(), " 5 JUL 2005"), FALSE)
reg_date_exact <- function(only = TRUE) {
  reg <- paste(reg_day(), reg_month(), reg_year())
  if(only) reg <- anchor_it(reg)
  reg
}


#' Construct a regular expression for DATE values
#'
#' @details The DATE (and subsequent DATE_CALENDAR) pattern can potentially handle several
#' different calendar types, but this package has only implemented the Gregorian calendar.
#' @param flatten A logical value which determines whether a single regex string should be
#' returned (flatten = TRUE) or if a vector of them should be returned (flatten = FALSE).
#' The vector output is used if the regexes need to be combined with other regexes. If they
#' do not, then they are anchored with ^ and $ and separated with | (OR).
#' @param only Whether to allow strings of only date. If FALSE,
#' the regular expression accepts patterns where text can come before or after
#' the date().
#' @return Either a single regex string or a vector of them
#' @export
#' @tests
#' expect_equal(grepl(reg_date(), "14 JAN 2005"), TRUE)
#' expect_equal(grepl(reg_date(), "14 JAM 2005"), FALSE)
#' expect_equal(grepl(reg_date(), "JAN 2005"), TRUE)
#' expect_equal(grepl(reg_date(), "5 JUL 2005"), TRUE)
#' expect_equal(grepl(reg_date(), "8 NOV 1956/57"), FALSE)
#' expect_equal(grepl(reg_date(), "2005"), TRUE)
#' expect_equal(grepl(reg_date(), "15 NOV 125"), TRUE)
#' expect_equal(grepl(reg_date(), "5 JUL 2005 "), FALSE)
#' expect_equal(grepl(reg_date(), " 5 JUL 2005"), FALSE)
reg_date <- function(flatten = TRUE, only = TRUE) {
  reg_date_gregorian(flatten, only)
}


reg_date_gregorian <- function(flatten = TRUE, only = TRUE) {
  combos <- c(reg_year(),
              paste(reg_year(), "BCE"),
              paste(reg_month(), reg_year()),
              paste(reg_day(), reg_month(), reg_year()))
  
  if(only) combos <- anchor_it(combos)
  if(flatten) combos <- paste(combos, collapse = "|")
  
  combos
}

#' Construct the regex pattern for DATE_PERIOD values
#'
#' @param flatten A logical value which determines whether a single regex string should be
#' returned (flatten = TRUE) or if a vector of them should be returned (flatten = FALSE).
#' The vector output is used if the regexes need to be combined with other regexes. If they
#' do not, then they are anchored with ^ and $ and separated with | (OR).
#' @return Either a single regex string or a vector of them
#' @tests
#' expect_equal(grepl(reg_date_period(), ""), TRUE)
#' expect_equal(grepl(reg_date_period(), "FROM 14 JAN 2005"), TRUE)
#' expect_equal(grepl(reg_date_period(), "TO 14 JAM 2005"), FALSE)
#' expect_equal(grepl(reg_date_period(), "FROM JAN 2005"), TRUE)
#' expect_equal(grepl(reg_date_period(), "FROM 14 JAN 2005/06 TO 2007"), FALSE)
#' expect_equal(grepl(reg_date_period(), "TO 5 JUL 2005"), TRUE)
#' expect_equal(grepl(reg_date_period(), "TO  8 NOV 1956"), FALSE)
#' expect_equal(grepl(reg_date_period(), "FROM 2005"), TRUE)
#' expect_equal(grepl(reg_date_period(), "FROM 15 NOV 125"), TRUE)
#' expect_equal(grepl(reg_date_period(), " TO JAN 1901"), FALSE)
#' expect_equal(grepl(reg_date_period(), "FROM 5 JUL 2005 "), FALSE)
#' expect_equal(grepl(reg_date_period(), " TO 5 JUL 2005"), FALSE)
reg_date_period <- function(flatten = TRUE) {
  combos <- c(paste("FROM", reg_date(FALSE,FALSE)),
              paste("TO", reg_date(FALSE,FALSE)),
              regex_combn(paste("FROM", reg_date(FALSE,FALSE)), 
                          paste(" TO", reg_date(FALSE,FALSE))),
              "") #date period can be the empty string
  if (flatten) {
    combos |> anchor_it() |> paste(collapse = "|")
  } else {
    combos
  }
}

#' Construct the regex pattern for DATE_RANGE values
#'
#' @param flatten A logical value which determines whether a single regex string should be
#' returned (flatten = TRUE) or if a vector of them should be returned (flatten = FALSE).
#' The vector output is used if the regexes need to be combined with other regexes. If they
#' do not, then they are anchored with ^ and $ and separated with | (OR).
#' @return Either a single regex string or a vector of them
#' @tests
#' expect_equal(grepl(reg_date_range(), "BEF 14 JAN 2005"), TRUE)
#' expect_equal(grepl(reg_date_range(), "AFT 14 JAM 2005"), FALSE)
#' expect_equal(grepl(reg_date_range(), "BEF JAN 2005"), TRUE)
#' expect_equal(grepl(reg_date_range(), "BET 14 JAN 2005/06 AND 2007"), FALSE)
#' expect_equal(grepl(reg_date_range(), "AFT 5 JUL 2005"), TRUE)
#' expect_equal(grepl(reg_date_range(), "AFT  8 NOV 1956"), FALSE)
#' expect_equal(grepl(reg_date_range(), "BEF 2005"), TRUE)
#' expect_equal(grepl(reg_date_range(), "BEF 15 NOV 125"), TRUE)
#' expect_equal(grepl(reg_date_range(), " AFT JAN 1901"), FALSE)
#' expect_equal(grepl(reg_date_range(), "BEF 5 JUL 2005 "), FALSE)
#' expect_equal(grepl(reg_date_range(), " AFT 5 JUL 2005"), FALSE)
reg_date_range <- function(flatten = TRUE) {
  combos <- c(paste("BEF", reg_date(FALSE,FALSE)),
              paste("AFT", reg_date(FALSE,FALSE)),
              regex_combn(paste("BET", reg_date(FALSE,FALSE)), 
                          paste(" AND", reg_date(FALSE,FALSE))))
  if (flatten) {
    combos |> anchor_it() |> paste(collapse = "|")
  } else {
    combos
  }
}

#' Construct the regex pattern for DATE_APPROXIMATED values
#'
#' @param flatten A logical value which determines whether a single regex string should be
#' returned (flatten = TRUE) or if a vector of them should be returned (flatten = FALSE).
#' The vector output is used if the regexes need to be combined with other regexes. If they
#' do not, then they are anchored with ^ and $ and separated with | (OR).
#' @return Either a single regex string or a vector of them
#' @tests
#' expect_equal(grepl(reg_date_approximated(), "ABT 14 JAN 2005"), TRUE)
#' expect_equal(grepl(reg_date_approximated(), "CAL 14 JAM 2005"), FALSE)
#' expect_equal(grepl(reg_date_approximated(), "EST JAN 2005"), TRUE)
#' expect_equal(grepl(reg_date_approximated(), "ABT 14 JAN 2005 AND 2007"), FALSE)
#' expect_equal(grepl(reg_date_approximated(), "EST 5 JUL 2005"), TRUE)
#' expect_equal(grepl(reg_date_approximated(), "CAL  8 NOV 1956"), FALSE)
#' expect_equal(grepl(reg_date_approximated(), "ABT 2005"), TRUE)
#' expect_equal(grepl(reg_date_approximated(), "CAL 15 NOV 125"), TRUE)
#' expect_equal(grepl(reg_date_approximated(), " EST JAN 1901"), FALSE)
#' expect_equal(grepl(reg_date_approximated(), "CAL 5 JUL 2005 "), FALSE)
#' expect_equal(grepl(reg_date_approximated(), " CAL 5 JUL 2005"), FALSE)
reg_date_approximated <- function(flatten = TRUE) {
  combos <- c(paste("ABT", reg_date(FALSE,FALSE)),
              paste("CAL", reg_date(FALSE,FALSE)),
              paste("EST", reg_date(FALSE,FALSE)))
  if (flatten) {
    combos |> anchor_it() |> paste(collapse = "|")
  } else {
    combos
  }
}

#' Construct the regex pattern for DATE_VALUE values
#'
#' @return Either a single regex string or a vector of them
#' @tests
#' expect_equal(grepl(reg_date_value(), "14 JAN 2005"), TRUE)
#' expect_equal(grepl(reg_date_value(), "MAR 1901"), TRUE)
#' expect_equal(grepl(reg_date_value(), "2010"), TRUE)
#' expect_equal(grepl(reg_date_value(), "FROM 14 FEB 2005"), TRUE)
#' expect_equal(grepl(reg_date_value(), "TO JAN 2005"), TRUE)
#' expect_equal(grepl(reg_date_value(), "FROM 14 JAN 2005/06 TO 2007"), FALSE)
#' expect_equal(grepl(reg_date_value(), "BEF 5 JUL 2005"), TRUE)
#' expect_equal(grepl(reg_date_value(), "AFT 8 NOV 1956/57"), FALSE)
#' expect_equal(grepl(reg_date_value(), "BET 2005 AND MAR 2008"), TRUE)
#' expect_equal(grepl(reg_date_value(), "CAL 15 NOV 1925"), TRUE)
#' expect_equal(grepl(reg_date_value(), "EST JAN 1901/58"), FALSE)
#' expect_equal(grepl(reg_date_value(), "ABT 5 JUL 2005"), TRUE)
#' expect_equal(grepl(reg_date_value(), "14 JAN 205"), TRUE)
#' expect_equal(grepl(reg_date_value(), "MAR 1901 "), FALSE)
#' expect_equal(grepl(reg_date_value(), " 2010"), FALSE)
#' expect_equal(grepl(reg_date_value(), "FROM 14 FEBR 2005"), FALSE)
#' expect_equal(grepl(reg_date_value(), "TO  JAN 2005"), FALSE)
#' expect_equal(grepl(reg_date_value(), "FROM 14 JAN 2005 AND 2007"), FALSE)
#' expect_equal(grepl(reg_date_value(), "BEF 5 JUL 2005 "), FALSE)
#' expect_equal(grepl(reg_date_value(), "AFT 8 NOV 1956/1957"), FALSE)
#' expect_equal(grepl(reg_date_value(), "BET 2005 TO MAR 2008"), FALSE)
#' expect_equal(grepl(reg_date_value(), "CAL 15 NOV 1925/"), FALSE)
#' expect_equal(grepl(reg_date_value(), "14TH JAN 1901"), FALSE)
#' expect_equal(grepl(reg_date_value(), "ABT 5  JUL 2005"), FALSE)
reg_date_value <- function() {
  
  c(reg_date(FALSE,FALSE),
    reg_date_period(FALSE),
    reg_date_range(FALSE),
    reg_date_approximated(FALSE)) |> 
    anchor_it() |> 
    paste(collapse = "|")
}

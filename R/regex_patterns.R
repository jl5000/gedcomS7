

group_it <- function(reg) {
  paste0("(?:", reg, ")")
}


anchor_it <- function(reg) {
  paste0("^", reg, "$")
}

reg_ged_line <- function(){
  # \\1 is level, \\2 is xref, \\3 is tag, \\4 is value
  sprintf("^([0-6])(?: (%s))? ([A-Z1-3]{3,5})(?: (.+))?$", reg_xref(FALSE))
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
  "\\d{1,2}" |> group_it()
}

reg_month <- function() {
  paste0(toupper(month.abb), collapse = "|") |> group_it()
}

reg_year <- function() {
  "\\d{3,4}" |> group_it()
}

reg_year_dual <- function() {
  "\\d{4}/\\d{2}" |> group_it()
}

reg_bce <- function() {
  "BCE|BC|B\\.C\\." |> group_it()
}

reg_time <- function(only = TRUE){
  hh <- paste(0:23, collapse = "|")
  mm <- paste(formatC(0:59, width = 2, format = "d", flag = "0"), collapse = "|")
  ss <- mm
  fs <- paste(formatC(0:99, width = 2, format = "d", flag = "0"), collapse = "|")
  reg <- sprintf("(%s):(%s)(:(%s)(\\.(%s))?)?", hh, mm, ss, fs)
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
  #p31
  reg <- "@[a-zA-Z0-9]{1,20}@"
  if(only) reg <- anchor_it(reg)
  reg
}

reg_latitude <- function() {
  "^[NS]\\d{1,2}(\\.\\d{1,6})?$"
}

reg_longitude <- function() {
  "^[EW]\\d{1,3}(\\.\\d{2,6})?$"
}

reg_age_at_event <- function() {
  paste0("^(?:[<>] )?",
         c("\\d{1,3}y \\d{1,2}m \\d{1,3}d$",
           "\\d{1,3}y \\d{1,2}m$",
           "\\d{1,3}y \\d{1,3}d$",
           "\\d{1,2}m \\d{1,3}d$",
           "\\d{1,3}y$",
           "\\d{1,2}m$",
           "\\d{1,3}d$")) |> 
    paste(collapse = "|")
  
}

reg_role_in_event <- function(){
  paste(anchor_it(val_roles()) , collapse = "|")
}

#' Construct a regular expression for a custom value
#' 
#' @details Custom values are allowed for date values and roles in events.
#' They are accepted for existing GEDCOM files, but they are not permitted to be created
#' in the gedcompendium.
#'
#' @return A regular expression pattern for a custom value.
#' @export
reg_custom_value <- function(){
  "^\\(.+\\)$"
}

#' Construct a regular expression for DATE_EXACT values
#' 
#' @param only Whether to allow strings of only date_exact. If FALSE,
#' the regular expression accepts patterns where text can come before or after
#' the date_exact().
#'
#' @tests
#' expect_equal(grepl(reg_date_exact(), "14 JAN 2005"), TRUE)
#' expect_equal(grepl(reg_date_exact(), "14 JAM 2005"), FALSE)
#' expect_equal(grepl(reg_date_exact(), "JAN 2005"), FALSE)
#' expect_equal(grepl(reg_date_exact(), "14 JAN 2005/06"), FALSE)
#' expect_equal(grepl(reg_date_exact(), "5 JUL 2005"), TRUE)
#' expect_equal(grepl(reg_date_exact(), "8 NOV 1956/57"), FALSE)
#' expect_equal(grepl(reg_date_exact(), "2005"), FALSE)
#' expect_equal(grepl(reg_date_exact(), "15 NOV 125"), FALSE)
#' expect_equal(grepl(reg_date_exact(), "JAN 1901/58"), FALSE)
#' expect_equal(grepl(reg_date_exact(), "5 JUL 2005 "), FALSE)
#' expect_equal(grepl(reg_date_exact(), " 5 JUL 2005"), FALSE)
#' @return A regex string
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
#' @tests
#' expect_equal(grepl(reg_date(), "14 JAN 2005"), TRUE)
#' expect_equal(grepl(reg_date(), "14 JAM 2005"), FALSE)
#' expect_equal(grepl(reg_date(), "JAN 2005"), TRUE)
#' expect_equal(grepl(reg_date(), "14 JAN 2005/06"), TRUE)
#' expect_equal(grepl(reg_date(), "5 JUL 2005"), TRUE)
#' expect_equal(grepl(reg_date(), "8 NOV 1956/57"), TRUE)
#' expect_equal(grepl(reg_date(), "2005"), TRUE)
#' expect_equal(grepl(reg_date(), "15 NOV 125"), FALSE)
#' expect_equal(grepl(reg_date(), "JAN 1901/58"), TRUE)
#' expect_equal(grepl(reg_date(), "5 JUL 2005 "), FALSE)
#' expect_equal(grepl(reg_date(), " 5 JUL 2005"), FALSE)
#' @return Either a single regex string or a vector of them
#' @export
reg_date <- function(flatten = TRUE, only = TRUE) {
  reg_date_calendar(flatten, only)
}

reg_date_calendar <- function(flatten = TRUE, only = TRUE) {
  reg_date_gregorian(flatten, only)
}

reg_date_gregorian <- function(flatten = TRUE, only = TRUE) {
  combos <- c(reg_year(),
              paste(reg_year(), reg_bce()),
              paste(reg_month(), reg_year()),
              paste(reg_day(), reg_month(), reg_year()),
              paste(reg_day(), reg_month()),
              paste(reg_month(), reg_year_dual()),
              paste(reg_day(), reg_month(), reg_year_dual()))
  
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
#' @tests
#' expect_equal(grepl(reg_date_period(), "FROM 14 JAN 2005"), TRUE)
#' expect_equal(grepl(reg_date_period(), "TO 14 JAM 2005"), FALSE)
#' expect_equal(grepl(reg_date_period(), "FROM JAN 2005"), TRUE)
#' expect_equal(grepl(reg_date_period(), "FROM 14 JAN 2005/06 TO 2007"), TRUE)
#' expect_equal(grepl(reg_date_period(), "TO 5 JUL 2005"), TRUE)
#' expect_equal(grepl(reg_date_period(), "TO  8 NOV 1956/57"), FALSE)
#' expect_equal(grepl(reg_date_period(), "FROM 2005"), TRUE)
#' expect_equal(grepl(reg_date_period(), "FROM 15 NOV 125"), FALSE)
#' expect_equal(grepl(reg_date_period(), " TO JAN 1901/58"), FALSE)
#' expect_equal(grepl(reg_date_period(), "FROM 5 JUL 2005 "), FALSE)
#' expect_equal(grepl(reg_date_period(), " TO 5 JUL 2005"), FALSE)
#' @return Either a single regex string or a vector of them
reg_date_period <- function(flatten = TRUE) {
  combos <- c(paste("FROM", reg_date(FALSE,FALSE)),
              paste("TO", reg_date(FALSE,FALSE)),
              regex_combn(paste("FROM", reg_date(FALSE,FALSE)), 
                          paste(" TO", reg_date(FALSE,FALSE))))
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
#' @tests
#' expect_equal(grepl(reg_date_range(), "BEF 14 JAN 2005"), TRUE)
#' expect_equal(grepl(reg_date_range(), "AFT 14 JAM 2005"), FALSE)
#' expect_equal(grepl(reg_date_range(), "BEF JAN 2005"), TRUE)
#' expect_equal(grepl(reg_date_range(), "BET 14 JAN 2005/06 AND 2007"), TRUE)
#' expect_equal(grepl(reg_date_range(), "AFT 5 JUL 2005"), TRUE)
#' expect_equal(grepl(reg_date_range(), "AFT  8 NOV 1956/57"), FALSE)
#' expect_equal(grepl(reg_date_range(), "BEF 2005"), TRUE)
#' expect_equal(grepl(reg_date_range(), "BEF 15 NOV 125"), FALSE)
#' expect_equal(grepl(reg_date_range(), " AFT JAN 1901/58"), FALSE)
#' expect_equal(grepl(reg_date_range(), "BEF 5 JUL 2005 "), FALSE)
#' expect_equal(grepl(reg_date_range(), " AFT 5 JUL 2005"), FALSE)
#' @return Either a single regex string or a vector of them
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
#' @tests
#' expect_equal(grepl(reg_date_approximated(), "ABT 14 JAN 2005"), TRUE)
#' expect_equal(grepl(reg_date_approximated(), "CAL 14 JAM 2005"), FALSE)
#' expect_equal(grepl(reg_date_approximated(), "EST JAN 2005"), TRUE)
#' expect_equal(grepl(reg_date_approximated(), "ABT 14 JAN 2005/06 AND 2007"), FALSE)
#' expect_equal(grepl(reg_date_approximated(), "EST 5 JUL 2005"), TRUE)
#' expect_equal(grepl(reg_date_approximated(), "CAL  8 NOV 1956/57"), FALSE)
#' expect_equal(grepl(reg_date_approximated(), "ABT 2005"), TRUE)
#' expect_equal(grepl(reg_date_approximated(), "CAL 15 NOV 125"), FALSE)
#' expect_equal(grepl(reg_date_approximated(), " EST JAN 1901/58"), FALSE)
#' expect_equal(grepl(reg_date_approximated(), "CAL 5 JUL 2005 "), FALSE)
#' expect_equal(grepl(reg_date_approximated(), " CAL 5 JUL 2005"), FALSE)
#' @return Either a single regex string or a vector of them
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
#' @tests
#' expect_equal(grepl(reg_date_value(), "14 JAN 2005"), TRUE)
#' expect_equal(grepl(reg_date_value(), "MAR 1901"), TRUE)
#' expect_equal(grepl(reg_date_value(), "2010"), TRUE)
#' expect_equal(grepl(reg_date_value(), "FROM 14 FEB 2005"), TRUE)
#' expect_equal(grepl(reg_date_value(), "TO JAN 2005"), TRUE)
#' expect_equal(grepl(reg_date_value(), "FROM 14 JAN 2005/06 TO 2007"), TRUE)
#' expect_equal(grepl(reg_date_value(), "BEF 5 JUL 2005"), TRUE)
#' expect_equal(grepl(reg_date_value(), "AFT 8 NOV 1956/57"), TRUE)
#' expect_equal(grepl(reg_date_value(), "BET 2005 AND MAR 2008"), TRUE)
#' expect_equal(grepl(reg_date_value(), "CAL 15 NOV 1925"), TRUE)
#' expect_equal(grepl(reg_date_value(), "EST JAN 1901/58"), TRUE)
#' expect_equal(grepl(reg_date_value(), "ABT 5 JUL 2005"), TRUE)
#' expect_equal(grepl(reg_date_value(), "14 JAN 205"), FALSE)
#' expect_equal(grepl(reg_date_value(), "MAR 1901 "), FALSE)
#' expect_equal(grepl(reg_date_value(), " 2010"), FALSE)
#' expect_equal(grepl(reg_date_value(), "FROM 14 FEBR 2005"), FALSE)
#' expect_equal(grepl(reg_date_value(), "TO  JAN 2005"), FALSE)
#' expect_equal(grepl(reg_date_value(), "FROM 14 JAN 2005/06 AND 2007"), FALSE)
#' expect_equal(grepl(reg_date_value(), "BEF 5 JUL 2005 "), FALSE)
#' expect_equal(grepl(reg_date_value(), "AFT 8 NOV 1956/1957"), FALSE)
#' expect_equal(grepl(reg_date_value(), "BET 2005 TO MAR 2008"), FALSE)
#' expect_equal(grepl(reg_date_value(), "CAL 15 NOV 1925/"), FALSE)
#' expect_equal(grepl(reg_date_value(), "14TH JAN 1901/58"), FALSE)
#' expect_equal(grepl(reg_date_value(), "ABT 5  JUL 2005"), FALSE)
#' @return Either a single regex string or a vector of them
reg_date_value <- function() {
  
  c(reg_date(FALSE,FALSE),
    reg_date_period(FALSE),
    reg_date_range(FALSE),
    reg_date_approximated(FALSE),
    reg_custom_value()) |> 
    anchor_it() |> 
    paste(collapse = "|")
}



group_it <- function(reg) {
  paste0("(?:", reg, ")")
}


anchor_it <- function(reg) {
  paste0("^", reg, "$")
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
  sprintf("^([0-9])(?: (%s))? (%s|%s)(?: (.+))?$", 
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
  reg <- "@[A-Z0-9_]+@"
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

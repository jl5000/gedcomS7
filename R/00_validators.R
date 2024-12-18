# This contains all validation functions for classes and therefore must appear
# before them in filename

#' Validate the size of an input
#'
#' @param input The input.
#' @param min_len The minimum number of elements the input should have.
#' @param max_len The maximum number of elements the input should have.
#' @param min_val The minimum number of characters or value the input should have
#' (depending on input type).
#' @param max_val The maximum number of characters or value the input should have
#' (depending on input type).
#'
#' @return Either a character string giving an error message, or NULL.
#' @tests
#' expect_equal(chk_input_size(letters, min_len = 27),
#'                             "has too few elements. The minimum is 27.")
#' expect_equal(chk_input_size(1:4, max_len = 3),
#'                             "has too many elements. The maximum is 3.")
#' expect_equal(chk_input_size(c("hello","once again"), min_val = 6),
#'                             "has too few characters. The minimum is 6.")
#' expect_equal(chk_input_size(c("goodbye","to you"), max_val = 6),
#'                             "has too many characters. The maximum is 6.")
#' expect_equal(chk_input_size(18:36, min_val = 19),
#'                             "has a value which is too low. The minimum is 19.")
#' expect_equal(chk_input_size(2:9, max_val = 8),
#'                             "has a value which is too high. The maximum is 8.")
#' expect_null(chk_input_size(letters, min_len = 26))
#' expect_null(chk_input_size(letters, max_len = 26))
#' expect_null(chk_input_size(month.abb, min_val = 3))
#' expect_null(chk_input_size(month.abb, max_val = 3))
#' expect_null(chk_input_size(20:40, min_val = 20))
#' expect_null(chk_input_size(20:40, max_val = 40))
chk_input_size <- function(input, 
                           min_len = NULL, 
                           max_len = NULL, 
                           min_val = NULL, 
                           max_val = NULL) {
  
  if (!is.null(min_len) && length(input) < min_len) 
    return(sprintf("has too few elements. The minimum is %s.", min_len))
  
  if (!is.null(max_len) && length(input) > max_len) 
    return(sprintf("has too many elements. The maximum is %s.", max_len))
  
  if (length(input) > 0 && !is.null(min_val) && is.character(input) && min(nchar(input)) < min_val) 
    return(sprintf("has too few characters. The minimum is %s.", min_val))
  
  if (length(input) > 0 && !is.null(max_val) && is.character(input) && max(nchar(input)) > max_val) 
    return(sprintf("has too many characters. The maximum is %s.", max_val))
  
  if (length(input) > 0 && !is.null(min_val) && is.numeric(input) && min(input) < min_val) 
    return(sprintf("has a value which is too low. The minimum is %s.", min_val))
  
  if (length(input) > 0 && !is.null(max_val) && is.numeric(input) && max(input) > max_val) 
    return(sprintf("has a value which is too high. The maximum is %s.", max_val))
    
  NULL
}


#' Validate the regex pattern(s) of a character vector
#'
#' @inheritParams chk_input_size
#' @param pattern The regex pattern each element must match.
#'
#' @inherit chk_input_size return
#' @tests
#' expect_equal(chk_input_pattern(letters, "[a-y]"),
#'              "is in an invalid format: z")
#' expect_equal(chk_input_pattern(month.abb, "[A-Z][a-z][abe-z]"),
#'              "is in an invalid format: Dec")
#' expect_equal(chk_input_pattern(month.abb, "[A-Z][a-z][a-xz]"),
#'              "is in an invalid format: May")
chk_input_pattern <- function(input, pattern) {
  if (length(input) > 0 && is.character(input)) {
    for (i in input) {
      if (!grepl(pattern, i))
        return(sprintf("is in an invalid format: %s", i))
    }
  }
  NULL
}


#' Validate an input against a set of valid values.
#'
#' @inheritParams chk_input_size 
#' @param choices A vector of valid values.
#'
#' @inherit chk_input_size return
#' @tests
#' expect_match(chk_input_choice(month.abb, month.abb[-6]),
#'              "has an invalid value: Jun")
#' expect_match(chk_input_choice(letters, letters[-16]),
#'              "has an invalid value: p")
#' expect_match(chk_input_choice(LETTERS, LETTERS[-20]),
#'              "has an invalid value: T")
chk_input_choice <- function(input, choices) {
  if (length(input) > 0 && is.character(input)){
    for (i in input) {
      if (!i %in% choices)
        return(sprintf("has an invalid value: %s\n  The valid values are: %s", 
                                 i, toString(choices)))
    }
  } 
  NULL
}


#' Validate a list to ensure it contains elements of the correct S7 class
#'
#' @inheritParams chk_input_size 
#' @param target_class The S7 class that the elements of the list should contain.
#' @param backup_pattern Need to handle List of mixed stuff vs character vector vs list of characters
#'
#' @inherit chk_input_size return
chk_input_S7classes <- function(input, target_class, backup_pattern = NULL){
  
  if("S7_object" %in% class(input)){
    if(!S7::S7_inherits(input, target_class))
      return(sprintf("contains an invalid object not of %s.", 
                     target_class@name))
  } else {
    for(inp in input){
      if(is.character(inp) && !is.null(backup_pattern)){
        if(!grepl(backup_pattern, inp))
          return("is in an invalid format.")
      } else {
        if(!S7::S7_inherits(inp, target_class))
          return(sprintf("contains an invalid object not of %s.", 
                         target_class@name))
      }
    }
  }
  
  NULL
}


#' Validate a date by its components
#' 
#' @param year The year.
#' @param month The month.
#' @param day The day.
#' @param bce Whether the date occurs before the common era.
#'
#' @inherit chk_input_size return
#' @tests
#' expect_equal(chk_input_date_cpts(numeric(),1,2),
#'              "Year must be defined")
#' expect_equal(chk_input_date_cpts(2000,1,2, bce = TRUE),
#'              "BCE date must contain year only")
#' expect_equal(chk_input_date_cpts(2000,numeric(),2),
#'              "Day is defined without a month")
#' expect_equal(chk_input_date_cpts(2000,13,2),
#'              "Invalid date")
#' expect_equal(chk_input_date_cpts(2001,2,29),
#'              "Invalid date")
#' expect_null(chk_input_date_cpts(2000,2,29))
#' expect_null(chk_input_date_cpts(2020,6,14))
#' expect_null(chk_input_date_cpts(1980,9,3))
chk_input_date_cpts <- function(year, month, day, bce = FALSE){
  if (length(year) == 0)
    return("Year must be defined")
  
  if(bce){
    if(length(year) == 0 || length(month) + length(day) > 0)
      return("BCE date must contain year only")
  }
  
  if (length(month) < length(day))
    return("Day is defined without a month")
  
  if(length(year) == 0) year <- 2000
  if(length(month) == 0) month <- 10
  if(length(day) == 0) day <- 10
  if(nchar(day) == 1) day <- paste0(0, day)
  if(nchar(month) == 1) month <- paste0(0, month)
  d <- try(as.Date(paste(day, month, year), format = "%d %m %Y"))
  if("try-error" %in% class(d) || is.na(d)) {
    return("Invalid date")
  }
  NULL
}

#' Validate a date range/period
#' 
#' @param start_date The start date given either as a `DateGregorian` or a GEDCOM date string.
#' @param end_date The end date given either as a `DateGregorian` or a GEDCOM date string.
#'
#' @inherit chk_input_size return
#' @tests
#' expect_null(chk_input_dates(NULL,NULL))
#' expect_null(chk_input_dates("2002-09-08",NULL))
chk_input_dates <- function(start_date, end_date){

  if(length(start_date) + length(end_date) < 2) return()
  
  start_val <- obj_to_val(start_date)
  end_val <- obj_to_val(end_date)
  
  start <- parse_gedcom_date(start_val, minimise = TRUE)
  end <- parse_gedcom_date(end_val, minimise = FALSE)
  
  if(start == end)
    return("Start date is the same as end date")
  
  if(start > end)
    return("Start date comes after end date")
  
  NULL
}


#' Validate an numeric input to be a whole number
#'
#' @inheritParams chk_input_size 
#'
#' @inherit chk_input_size return
#' @tests
#' expect_equal(chk_whole_number(1.1),
#'              "must be a whole number")
#' expect_equal(chk_whole_number(4.2),
#'              "must be a whole number")
#' expect_null(chk_whole_number(1:4))
#' expect_null(chk_whole_number(5))
chk_whole_number <- function(input){
  if(is.numeric(input) && length(input) == 1 && floor(input) != input)
    return("must be a whole number")
  
  NULL
}


#' Validate the existence of an input's parent value
#'
#' @inheritParams chk_input_size 
#' @param name The input name.
#' @param parent The parent input.
#' @param parent_name The name of the parent input used in any error messages.
#'
#' @inherit chk_input_size return
#' @tests
#' expect_null(chk_input_parents(character(), "@child", character(), "@parent"))
#' expect_null(chk_input_parents(character(), "@child", "parent", "@parent"))
#' expect_equal(chk_input_parents("child", "@child", character(), "@parent"),
#'              "@child requires a @parent")
#' expect_null(chk_input_parents("child", "@child", "parent", "@parent"))
chk_input_parents <- function(input, name, parent, parent_name){
  if(length(parent) == 0 && length(input) > 0)
    return(sprintf("%s requires a %s", name, parent_name))
  
  NULL
}

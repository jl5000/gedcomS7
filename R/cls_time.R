#' @include cls_validators.R
NULL

#' Create a time object
#' 
#' @inheritParams prop_definitions
#' @return An S7 object representing a GEDCOM time.
#' @export
#' @tests
#' expect_error(class_time(), regexp = "@hour has too few elements.*@minute has too few elements")
#' expect_error(class_time(hour = 30), regexp = "@hour has a value which is too high.*@minute has too few elements")
#' expect_error(class_time(hour = 20, minute = 60), regexp = "@minute has a value which is too high.")
#' expect_error(class_time(hour = 10, minute = 10, fraction = 123), regexp = "@fraction has too many elements")
#' expect_error(class_time(hour = 10, minute = 2, utc = logical()), regexp = "@utc has too few elements")
#' expect_equal(class_time(hour = 10, minute = 59)@as_val, "10:59Z")
#' expect_equal(class_time(5, 6)@as_val, "05:06Z")
#' expect_equal(class_time(5, 59, 19)@as_val, "05:59:19Z")
#' expect_equal(class_time(0, 0, 0, 6)@as_val, "00:00:00.6Z")
#' expect_equal(class_time(8, 14, 43, 6543)@as_val, "08:14:43.6543Z")
#' expect_equal(class_time(14, 28, utc = FALSE)@as_val, "14:28")
class_time <- S7::new_class(
  "class_time",
  package = "gedcomS7",
  properties = list(
    hour = S7::class_numeric,
    minute = S7::class_numeric,
    second = S7::class_numeric,
    fraction = S7::class_numeric,
    utc = S7::new_property(S7::class_logical, default = TRUE),
    
    as_val = S7::new_property(
      S7::class_character,
      getter = function(self){
        tim <- sprintf("%02d:%02d", self@hour, self@minute)
        if(length(self@second) == 1)
          tim <- sprintf("%s:%02d", tim, self@second)
        if(length(self@fraction) == 1)
          tim <- paste0(tim, ".", self@fraction)
        if(self@utc)
          tim <- paste0(tim, "Z")
        tim
      }
    )
  ),
  validator = function(self){
    c(
      chk_whole_number(self@hour, "@hour"),
      chk_whole_number(self@minute, "@minute"),
      chk_whole_number(self@second, "@second"),
      chk_whole_number(self@fraction, "@fraction"),
      chk_input_size(self@hour, "@hour", 1, 1, 0, 23),
      chk_input_size(self@minute, "@minute", 1, 1, 0, 59),
      chk_input_size(self@second, "@second", 0, 1, 0, 59),
      chk_input_size(self@fraction, "@fraction", 0, length(self@second), 1),
      chk_input_size(self@utc, "@utc", 1, 1)
    )
  })
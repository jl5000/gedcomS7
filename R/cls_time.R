
#' Create a time object
#' 
#' @param hour The hour of the day given as an integer between 0 and 23.
#' @param minute The minute of the hour given as an integer between 0 and 59.
#' @param second The second of the minute given as an integer between 0 and 59.
#' @param fraction The fraction of the second given as an integer.
#' @param utc Whether the time is in Coordinated Universal Time (UTC) (TRUE, the default) or
#' is in local time (FALSE).
#' 
#' @returns An S7 object representing a GEDCOM time.
#' @export
#' @tests
#' expect_error(Time(), regexp = "@hour has too few elements.*@minute has too few elements")
#' expect_error(Time(hour = 30), regexp = "@hour has a value which is too high.*@minute has too few elements")
#' expect_error(Time(hour = 20, minute = 60), regexp = "@minute has a value which is too high.")
#' expect_error(Time(hour = 10, minute = 10, fraction = 123), regexp = "@fraction requires @second")
#' expect_error(Time(hour = 10, minute = 2, utc = logical()), regexp = "@utc has too few elements")
#' expect_equal(Time(hour = 10, minute = 59)@GEDCOM_STRING, "10:59Z")
#' expect_equal(Time(5, 6)@GEDCOM_STRING, "05:06Z")
#' expect_equal(Time(5, 59, 19)@GEDCOM_STRING, "05:59:19Z")
#' expect_equal(Time(0, 0, 0, 6)@GEDCOM_STRING, "00:00:00.6Z")
#' expect_equal(Time(8, 14, 43, 6543)@GEDCOM_STRING, "08:14:43.6543Z")
#' expect_equal(Time(14, 28, utc = FALSE)@GEDCOM_STRING, "14:28")
Time <- S7::new_class(
  "Time",
  parent = GedcomS7class,
  properties = list(
    hour = prop_whole(1, 1, 0, 23),
    minute = prop_whole(1, 1, 0, 59),
    second = prop_whole(0, 1, 0, 59),
    fraction = prop_whole(),
    utc = prop_bool(default = TRUE),
    
    GEDCOM_STRING = S7::new_property(
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
    if(length(self@fraction) == 1 && length(self@second) == 0)
      return("@fraction requires @second")
  })


S7::method(summary, Time) <- function(object, ...){
  to_console("Time:", object@GEDCOM_STRING, 15)
}

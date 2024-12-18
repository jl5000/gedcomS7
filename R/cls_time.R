
#' Create a time object
#' 
#' @inheritParams prop_definitions
#' @return An S7 object representing a GEDCOM time.
#' @export
#' @tests
#' expect_error(Time(), regexp = "@hour has too few elements.*@minute has too few elements")
#' expect_error(Time(hour = 30), regexp = "@hour has a value which is too high.*@minute has too few elements")
#' expect_error(Time(hour = 20, minute = 60), regexp = "@minute has a value which is too high.")
#' expect_error(Time(hour = 10, minute = 10, fraction = 123), regexp = "@fraction requires @second")
#' expect_error(Time(hour = 10, minute = 2, utc = logical()), regexp = "@utc has too few elements")
#' expect_equal(Time(hour = 10, minute = 59)@c_as_val, "10:59Z")
#' expect_equal(Time(5, 6)@c_as_val, "05:06Z")
#' expect_equal(Time(5, 59, 19)@c_as_val, "05:59:19Z")
#' expect_equal(Time(0, 0, 0, 6)@c_as_val, "00:00:00.6Z")
#' expect_equal(Time(8, 14, 43, 6543)@c_as_val, "08:14:43.6543Z")
#' expect_equal(Time(14, 28, utc = FALSE)@c_as_val, "14:28")
Time <- S7::new_class(
  "Time",
  properties = list(
    hour = S7::new_property(S7::class_numeric,
                            validator = function(value){
                              c(
                                chk_input_size(value, 1, 1, 0, 23),
                                chk_whole_number(value)
                              )
                            }),
    minute = S7::new_property(S7::class_numeric,
                              validator = function(value){
                                c(
                                  chk_input_size(value, 1, 1, 0, 59),
                                  chk_whole_number(value)
                                )
                              }),
    second = S7::new_property(S7::class_numeric,
                              validator = function(value){
                                c(
                                  chk_input_size(value, 0, 1, 0, 59),
                                  chk_whole_number(value)
                                )
                              }),
    fraction = S7::new_property(S7::class_numeric,
                                validator = function(value){
                                  chk_whole_number(value)
                                }),
    utc = S7::new_property(S7::class_logical, default = TRUE,
                           validator = function(value){
                             chk_input_size(value, 1, 1)
                           }),
    
    c_as_val = S7::new_property(
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

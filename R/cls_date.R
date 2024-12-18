
class_date <- S7::new_class("class_date", abstract = TRUE)


#' Create a GEDCOM Exact Date object
#' 
#' @inheritParams prop_definitions
#' @return An S7 object representing a GEDCOM Exact Date.
#' @export
#' @tests
#' expect_error(class_date_exact(), regexp = "@year has too few.*@month has too few.*@day has too few")
#' expect_error(class_date_exact(2001), regexp = "@month has too few.*@day has too few")
#' expect_error(class_date_exact(2001, 5), regexp = "@day has too few")
#' expect_error(class_date_exact(2001, 5, 32), regexp = "@day has a value which is too high")
#' expect_error(class_date_exact(2001, 2, 29), regexp = "Invalid date")
#' expect_equal(class_date_exact(2001, 5, 2)@c_as_val, "2 MAY 2001")
#' expect_equal(class_date_exact(28, 7, 12)@c_as_val, "12 JUL 28")
#' expect_equal(class_date_exact(28, 7, 12)@as_date, as.Date("28-07-12"))
class_date_exact <- S7::new_class(
  "class_date_exact", 
  parent = class_date,
  properties = list(
    year = S7::new_property(S7::class_numeric,
                            validator = function(value){
                              c(
                                chk_input_size(value, 1, 1, 1),
                                chk_whole_number(value)
                              )
                            }),
    month = S7::new_property(S7::class_numeric,
                             validator = function(value){
                               c(
                                 chk_input_size(value, 1, 1, 1, 12),
                                 chk_whole_number(value)
                               )
                             }),
    day = S7::new_property(S7::class_numeric,
                           validator = function(value){
                             c(
                               chk_input_size(value, 1, 1, 1, 31),
                               chk_whole_number(value)
                             )
                           }),
    
    c_as_val = S7::new_property(
      S7::class_character,
      getter = function(self){
        paste(self@day, toupper(month.abb[self@month]), self@year)
      }),
    
    as_date = S7::new_property(
      S7::class_Date,
      getter = function(self){
        day <- sprintf("%02d", self@day)
        month <- sprintf("%02d", self@month)
        year <- self@year
        as.Date(paste(day, month, year), format = "%d %m %Y")
      })
  ),
  validator = function(self){
    chk_input_date_cpts(self@year, self@month, self@day)
  }
)

#' Create a GEDCOM Exact Date object for today
#' 
#' @return An S7 object representing a GEDCOM Exact Date for today.
#' @export
#' @tests
#' expect_equal(date_exact_current()@as_date, Sys.Date())
date_exact_current <- function(){
  class_date_exact(year = as.integer(format(Sys.Date(), "%Y")),
                   month = as.integer(format(Sys.Date(), "%m")),
                   day = as.integer(format(Sys.Date(), "%d")))
}


#' Create a GEDCOM Gregorian Date object
#' 
#' @inheritParams prop_definitions
#' @return An S7 object representing a GEDCOM Gregorian Date.
#' @export
#' @tests
#' expect_error(class_date_greg(), regexp = "@year has too few elements")
#' expect_error(class_date_greg(2001, day = 15), regexp = "Day is defined without a month")
#' expect_error(class_date_greg(day = 5), regexp = "@year has too few elements")
#' expect_error(class_date_greg(month = 10), regexp = "@year has too few elements")
#' expect_error(class_date_greg(2010, 13, 3), regexp = "@month has a value which is too high")
#' expect_error(class_date_greg(2010, 1, 32), regexp = "@day has a value which is too high")
#' expect_error(class_date_greg(320, 5, 16, bce = TRUE), regexp = "BCE date must contain year only")
#' expect_equal(class_date_greg(2001, 5, 12)@c_as_val, "12 MAY 2001")
#' expect_equal(class_date_greg(2004, 2, 29)@c_as_val, "29 FEB 2004")
#' expect_equal(class_date_greg(2004, 8)@c_as_val, "AUG 2004")
#' expect_equal(class_date_greg(2012)@c_as_val, "2012")
#' expect_equal(class_date_greg(193, bce = TRUE)@c_as_val, "193 BCE")
class_date_greg <- S7::new_class(
  "class_date_greg", 
  parent = class_date,
  properties = list(
    year = S7::new_property(S7::class_numeric,
                            validator = function(value){
                              c(
                                chk_input_size(value, 1, 1, 1),
                                chk_whole_number(value)
                              )
                            }),
    month = S7::new_property(S7::class_numeric,
                             validator = function(value){
                               c(
                                 chk_input_size(value, 0, 1, 1, 12),
                                 chk_whole_number(value)
                               )
                             }),
    day = S7::new_property(S7::class_numeric,
                           validator = function(value){
                             c(
                               chk_input_size(value, 0, 1, 1, 31),
                               chk_whole_number(value)
                             )
                           }),
    bce = S7::new_property(S7::class_logical, default = FALSE,
                           validator = function(value){
                             chk_input_size(value, 1, 1)
                           }),
    
    c_as_val = S7::new_property(
      S7::class_character,
      getter = function(self){
        val <- ""
        if (length(self@day) == 1) 
          val <- paste(val, self@day)
        if (length(self@month) == 1) 
          val <- paste(val, toupper(month.abb[self@month]))
        if (length(self@year) == 1) {
          val <- paste(val, self@year)
          if(length(self@month) + length(self@day) == 0 & self@bce)
            val <- paste(val, "BCE")
        }
        trimws(val)
      }               
    )
  ),
  validator = function(self){
    chk_input_date_cpts(self@year, self@month, self@day, self@bce)
  }
)

#' Create a GEDCOM Approximate Date object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM Approximate Date.
#' @export
#' @tests
#' expect_error(class_date_approx("hello"), regexp = "@date_greg is in an invalid format")
#' expect_equal(class_date_approx(class_date_greg(2001, 5, 12), calc = TRUE)@c_as_val, 
#'                               "CAL 12 MAY 2001")
#' expect_equal(class_date_approx(class_date_greg(2004, 2, 29), about = TRUE)@c_as_val, 
#'                               "ABT 29 FEB 2004")
#' expect_equal(class_date_approx(class_date_greg(2004, 8), est = TRUE)@c_as_val, 
#'                                "EST AUG 2004")
class_date_approx <- S7::new_class(
  "class_date_approx", 
  parent = class_date,
  properties = list(
    date_greg = S7::new_property(S7::class_character | 
                                   S7::new_S3_class("gedcomS7::class_date_greg"),
                                 validator = function(value){
                                   c(
                                     chk_input_size(value, 1, 1),
                                     chk_input_pattern(value, reg_date_gregorian())
                                   )
                                 }),
    about = S7::new_property(S7::class_logical, default = TRUE,
                             validator = function(value){
                               chk_input_size(value, 1, 1)
                             }),
    calc = S7::new_property(S7::class_logical, default = FALSE,
                            validator = function(value){
                              chk_input_size(value, 1, 1)
                            }),
    est = S7::new_property(S7::class_logical, default = FALSE,
                           validator = function(value){
                             chk_input_size(value, 1, 1)
                           }),
    
    c_as_val = S7::new_property(
      S7::class_character,
      getter = function(self){
        if(self@calc) {
          paste("CAL", obj_to_val(self@date_greg))
        } else if(self@est) {
          paste("EST", obj_to_val(self@date_greg))
        } else if (self@about) {
          paste("ABT", obj_to_val(self@date_greg))
        } 
      }               
    )
  )
)


#' Create a GEDCOM Date Period object
#' 
#' @inheritParams prop_definitions
#' @return An S7 object representing a GEDCOM Date Period.
#' @export
#' @tests
#' expect_equal(class_date_period()@c_as_val, "")
#' expect_error(class_date_period(""), regexp = "@start_date is in an invalid format")
#' expect_error(class_date_period(end_date = ""), regexp = "@end_date is in an invalid format")
#' expect_equal(class_date_period("2 JUL 1989")@c_as_val, "FROM 2 JUL 1989")
#' expect_equal(class_date_period(end_date = "2 JUL 1989")@c_as_val, "TO 2 JUL 1989")
#' expect_equal(
#'   class_date_period(
#'     start_date = class_date_greg(1995, 6, 1)
#'   )@c_as_val, "FROM 1 JUN 1995")
#' expect_equal(
#'   class_date_period(
#'     end_date = class_date_greg(1995, 6, 1)
#'   )@c_as_val, "TO 1 JUN 1995")
#' expect_equal(
#'   class_date_period(
#'     start_date = class_date_greg(1990, 6, 1),
#'     end_date = class_date_greg(1995, 3)
#'   )@c_as_val, "FROM 1 JUN 1990 TO MAR 1995")
#' expect_error(
#'   class_date_period(
#'     start_date = class_date_greg(1995, 6, 1),
#'     end_date = class_date_greg(1995, 6, 1)
#'   ), regexp = "Start date is the same as end date")
#' expect_error(
#'   class_date_period(
#'     start_date = class_date_greg(2005, 6, 1),
#'     end_date = class_date_greg(1995, 6, 1)
#'   ), regexp = "Start date comes after end date")
#' expect_error(
#'   class_date_period(
#'     start_date = class_date_greg(2005, 8, 1),
#'     end_date = class_date_greg(2005, 6, 1)
#'   ), regexp = "Start date comes after end date")
#' expect_error(
#'   class_date_period(
#'     start_date = class_date_greg(2005, 8, 10),
#'     end_date = class_date_greg(2005, 8, 1)
#'   ), regexp = "Start date comes after end date")
class_date_period <- S7::new_class(
  "class_date_period", 
  parent = class_date,
  properties = list(
    start_date = S7::new_property(S7::class_character |
                                    S7::new_S3_class("gedcomS7::class_date_greg"),
                                  validator = function(value){
                                    c(
                                      chk_input_size(value, 0, 1),
                                      chk_input_pattern(value, reg_date_gregorian())
                                    )
                                  }),
    end_date = S7::new_property(S7::class_character |
                                  S7::new_S3_class("gedcomS7::class_date_greg"),
                                validator = function(value){
                                  c(
                                    chk_input_size(value, 0, 1),
                                    chk_input_pattern(value, reg_date_gregorian())
                                  )
                                }),
    
    c_as_val = S7::new_property(
      S7::class_character,
      getter = function(self){
        if (length(self@start_date) + length(self@end_date) == 2) {
          paste("FROM", obj_to_val(self@start_date), 
                "TO", obj_to_val(self@end_date))
        } else if (length(self@start_date) == 1) {
          paste("FROM", obj_to_val(self@start_date))
        } else if (length(self@end_date) == 1) {
          paste("TO", obj_to_val(self@end_date))
        } else {
          ""
        }
      }               
    )
  ),
  validator = function(self){
    chk_input_dates(self@start_date, self@end_date)
  }
)

#' Create a GEDCOM Date Range object
#' 
#' @inheritParams prop_definitions
#' @return An S7 object representing a GEDCOM Date Range.
#' @export
#' @tests
#' expect_error(class_date_range(), regexp = "has too few elements")
#' expect_error(class_date_range(""), regexp = "@start_date is in an invalid format")
#' expect_error(class_date_range(end_date = ""), regexp = "@end_date is in an invalid format")
#' expect_equal(class_date_range("2 JUL 1989")@c_as_val, "AFT 2 JUL 1989")
#' expect_equal(class_date_range(end_date = "2 JUL 1989")@c_as_val, "BEF 2 JUL 1989")
#' expect_equal(
#'   class_date_range(
#'     start_date = class_date_greg(1995, 6, 1)
#'   )@c_as_val, "AFT 1 JUN 1995")
#' expect_equal(
#'   class_date_range(
#'     end_date = class_date_greg(1995, 6, 1)
#'   )@c_as_val, "BEF 1 JUN 1995")
#' expect_equal(
#'   class_date_range(
#'     start_date = class_date_greg(1990, 6, 1),
#'     end_date = class_date_greg(1995, 3)
#'   )@c_as_val, "BET 1 JUN 1990 AND MAR 1995")
#' expect_error(
#'   class_date_range(
#'    start_date = class_date_greg(1995, 6, 1),
#'     end_date = class_date_greg(1995, 6, 1)
#'   ), regexp = "Start date is the same as end date")
#' expect_error(
#'   class_date_range(
#'     start_date = class_date_greg(2005, 6, 1),
#'     end_date = class_date_greg(1995, 6, 1)
#'   ), regexp = "Start date comes after end date")
#' expect_error(
#'   class_date_range(
#'     start_date = class_date_greg(2005, 8, 1),
#'     end_date = class_date_greg(2005, 6, 1)
#'   ), regexp = "Start date comes after end date")
#' expect_error(
#'   class_date_range(
#'     start_date = class_date_greg(2005, 8, 10),
#'     end_date = class_date_greg(2005, 8, 1)
#'   ), regexp = "Start date comes after end date")
class_date_range <- S7::new_class(
  "class_date_range", 
  parent = class_date_period,
  properties = list(
    
    c_as_val = S7::new_property(
      S7::class_character,
      getter = function(self){
        if (length(self@start_date) + length(self@end_date) == 2) {
          paste("BET", obj_to_val(self@start_date), 
                "AND", obj_to_val(self@end_date))
        } else if (length(self@start_date) == 1) {
          paste("AFT", obj_to_val(self@start_date))
        } else if (length(self@end_date) == 1) {
          paste("BEF", obj_to_val(self@end_date))
        } 
      }               
    )
  ),
  validator = function(self){
    # Date period can be empty, but not date range
    chk_input_size(Filter(\(x) length(x) != 0, list(self@start_date, self@end_date)), 
                   1, 2)
  }
)

#' Create a GEDCOM Date Value object
#' 
#' @inheritParams prop_definitions
#' @return An S7 object representing a GEDCOM Date Value.
#' @export
#' @tests
#' expect_error(class_date_value("FROM 2016", time = "12:34"), regexp = "A date period should not have a time defined")
#' expect_error(class_date_value(class_date_period(end_date = "1980"), time = class_time(3,45,54,6765)), 
#'              regexp = "A date period should not have a time defined")
#' expect_equal(class_date_value(class_date_greg(2005, 1, 5))@c_as_val, "5 JAN 2005")
#' expect_snapshot_value(class_date_value("AFT 1990", date_phrase = "Maybe 1992")@c_as_ged, "json2")
#' expect_snapshot_value(class_date_value("", date_phrase = "Phrase only", time = "02:24")@c_as_ged, "json2")
class_date_value <- S7::new_class(
  "class_date_value",
  parent = class_date,
  properties = list(
    date = S7::new_property(S7::class_character | 
                              S7::new_S3_class("gedcomS7::class_date_greg") | 
                              S7::new_S3_class("gedcomS7::class_date_period") |
                              S7::new_S3_class("gedcomS7::class_date_range") | 
                              S7::new_S3_class("gedcomS7::class_date_approx"),
                            validator = function(value){
                              c(
                                chk_input_size(value, 1, 1),
                                chk_input_pattern(value, reg_date_value())
                              )
                            }),
    date_phrase = S7::new_property(S7::class_character,
                                   validator = function(value){
                                     chk_input_size(value, 0, 1, 1)
                                   }),
    time = S7::new_property(S7::class_character | 
                              S7::new_S3_class("gedcomS7::class_time"),
                            validator = function(value){
                              c(
                                chk_input_size(value, 0, 1),
                                chk_input_pattern(value, reg_time())
                              )
                            }),
    
    c_as_val = S7::new_property(S7::class_character, 
                              getter = function(self) obj_to_val(self@date)),
    
    c_as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 DATE %s", obj_to_val(self@date)) |> trimws(),
          sprintf("1 TIME %s", obj_to_val(self@time)),
          sprintf("1 PHRASE %s", self@date_phrase)
        )
      })
  ),
  validator = function(self){
    #date_period has no time
    if(grepl("(FROM)|(TO)", obj_to_val(self@date))){
      if(length(self@time) > 0)
        return("A date period should not have a time defined.")
    }
  }
)

#' Create a GEDCOM Sorting Date object
#' 
#' @param date The date given either as a formatted GEDCOM string, or a
#' `class_date_greg` object.
#' @inheritParams prop_definitions
#' 
#' @return An S7 object representing a GEDCOM Sorting Date.
#' @export
#' @tests
#' expect_error(class_date_sort(""), regexp = "@date is in an invalid format")
#' expect_error(class_date_sort("FROM 2016"), regexp = "@date is in an invalid format")
#' expect_error(class_date_sort(class_date_period(end_date = "1980")), 
#'              regexp = "@date must be <character> or ")
#' expect_equal(class_date_sort(class_date_greg(2005, 1, 5))@c_as_val, "5 JAN 2005")
#' expect_snapshot_value(class_date_sort("1990", date_phrase = "Maybe 1992")@c_as_ged, "json2")
class_date_sort <- S7::new_class(
  "class_date_sort",
  parent = class_date_value,
  properties = list(
    date = S7::new_property(S7::class_character | 
                              S7::new_S3_class("gedcomS7::class_date_greg"),
                            validator = function(value){
                              chk_input_pattern(value, reg_date_gregorian())
                            }),
    
    c_as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 SDATE %s", obj_to_val(self@date)),
          sprintf("1 TIME %s", obj_to_val(self@time)),
          sprintf("1 PHRASE %s", self@date_phrase)
        )
      })
  )
)


parse_date_value <- function(lines, location = NULL, sorting = FALSE){
  if(sorting) tag <- "SDATE" else tag <- "DATE"
  date_val <- find_ged_values(lines, c(location, tag))
  
  if(length(date_val) == 0) return(character()) 
  
  if(sorting){
    dt <- class_date_sort(date = date_val)
  } else {
    dt <- class_date_value(date = date_val)
  }
  
  S7::props(dt) <- list(
    date_phrase = find_ged_values(lines, c(location, tag,"PHRASE")),
    time = find_ged_values(lines, c(location, tag,"TIME"))
  )
  
  dt
}

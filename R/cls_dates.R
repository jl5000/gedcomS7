#' @include cls_validators.R
NULL

#' @export
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
      chk_input_size(self@fraction, "@fraction", 0, 1, 1, 9),
      chk_input_size(self@utc, "@utc", 1, 1)
    )
  })

class_date <- S7::new_class("class_date")

#' @export
class_date_exact <- S7::new_class(
  "class_date_exact", 
  package = "gedcomS7",
  parent = class_date,
  properties = list(
    year = S7::class_numeric,
    month = S7::class_numeric,
    day = S7::class_numeric,
    
    as_val = S7::new_property(
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
    c(
      chk_whole_number(self@day, "@day"),
      chk_whole_number(self@month, "@month"),
      chk_whole_number(self@year, "@year"),
      chk_input_size(self@day, "@day", 1, 1, 1, 31),
      chk_input_size(self@month, "@month", 1, 1, 1, 12),
      chk_input_size(self@year, "@year", 1, 1, 1),
      chk_input_date_cpts(self@year, self@month, self@day)
    )
  }
)

#' @export
date_exact_current <- function(){
  class_date_exact(year = as.integer(format(Sys.Date(), "%Y")),
                   month = as.integer(format(Sys.Date(), "%m")),
                   day = as.integer(format(Sys.Date(), "%d")))
}


#' @export
class_date_calendar <- S7::new_class(
  "class_date_calendar", 
  package = "gedcomS7",
  parent = class_date,
  properties = list(
    year = S7::class_numeric,
    month = S7::class_numeric,
    day = S7::class_numeric,
    bce = S7::new_property(S7::class_logical, default = FALSE),
    
    as_val = S7::new_property(
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
    c(
      chk_whole_number(self@day, "@day"),
      chk_whole_number(self@month, "@month"),
      chk_whole_number(self@year, "@year"),
      chk_input_size(self@day, "@day", 0, 1, 1, 31),
      chk_input_size(self@month, "@month", 0, 1, 1, 12),
      chk_input_size(self@year, "@year", 1, 1, 1),
      chk_input_size(self@bce, "@bce", 1, 1),
      chk_input_date_cpts(self@year, self@month, self@day, self@bce)
    )
  }
)

#' @export
class_date_approx <- S7::new_class(
  "class_date_approx", 
  package = "gedcomS7",
  parent = class_date,
  properties = list(
    date = class_date_calendar | S7::class_character,
    about = S7::new_property(S7::class_logical, default = TRUE),
    calc = S7::new_property(S7::class_logical, default = FALSE),
    est = S7::new_property(S7::class_logical, default = FALSE),
    
    as_val = S7::new_property(
      S7::class_character,
      getter = function(self){
        if(self@calc) {
          paste("CAL", datetime_to_val(self@date))
        } else if(self@est) {
          paste("EST", datetime_to_val(self@date))
        } else if (self@about) {
          paste("ABT", datetime_to_val(self@date))
        } 
      }               
    )
  ),
  validator = function(self){
    c(
      chk_input_size(self@date, "@date", 1, 1),
      chk_input_size(self@about, "@about", 1, 1),
      chk_input_size(self@calc, "@calc", 1, 1),
      chk_input_size(self@est, "@est", 1, 1),
      chk_input_pattern(self@date, "@date", reg_date_calendar())
    )
  }
)


#' @export
class_date_period <- S7::new_class(
  "class_date_period", 
  package = "gedcomS7",
  parent = class_date,
  properties = list(
    start_date = NULL | class_date_calendar | S7::class_character,
    end_date = NULL | class_date_calendar | S7::class_character,
    
    as_val = S7::new_property(
      S7::class_character,
      getter = function(self){
        if (length(self@start_date) + length(self@end_date) == 2) {
          paste("FROM", datetime_to_val(self@start_date), 
                "TO", datetime_to_val(self@end_date))
        } else if (length(self@start_date) == 1) {
          paste("FROM", datetime_to_val(self@start_date))
        } else if (length(self@end_date) == 1) {
          paste("TO", datetime_to_val(self@end_date))
        } 
      }               
    )
  ),
  validator = function(self){
    c(
      chk_input_size(self@start_date, "@start_date", 0, 1),
      chk_input_size(self@end_date, "@end_date", 0, 1),
      chk_input_size(Filter(Negate(is.null), list(self@start_date, self@end_date)), 
                     "@start_date + @end_date", 1, 2),
      chk_input_pattern(self@start_date, "@start_date", reg_date_calendar()),
      chk_input_pattern(self@end_date, "@end_date", reg_date_calendar()),
      chk_input_dates(self@start_date, self@end_date)
    )
  }
)

#' @export
class_date_range <- S7::new_class(
  "class_date_range", 
  package = "gedcomS7",
  parent = class_date_period,
  properties = list(
    
    as_val = S7::new_property(
      S7::class_character,
      getter = function(self){
        if (length(self@start_date) + length(self@end_date) == 2) {
          paste("BET", datetime_to_val(self@start_date), 
                "AND", datetime_to_val(self@end_date))
        } else if (length(self@start_date) == 1) {
          paste("AFT", datetime_to_val(self@start_date))
        } else if (length(self@end_date) == 1) {
          paste("BEF", datetime_to_val(self@end_date))
        } 
      }               
    )
  )
)

#' @export
class_date_value <- S7::new_class(
  "class_date_value",
  package = "gedcomS7",
  parent = class_date,
  properties = list(
    date = NULL | class_date_calendar | class_date_period |
           class_date_range | class_date_approx | S7::class_character,
    date_phrase = S7::class_character,
    time = NULL | class_time | S7::class_character,
    sorting = S7::new_property(S7::class_logical, default = FALSE),
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        if(self@sorting) tag <- "SDATE" else tag <- "DATE"
        c(
          sprintf("0 %s %s", tag, datetime_to_val(self@date)),
          sprintf("1 TIME %s", datetime_to_val(self@time)),
          sprintf("1 PHRASE %s", self@date_phrase)
        )
      })
  ),
  validator = function(self){
    c( #dateperiod has no time
      chk_input_size(self@date, "@date", 1, 1),
      chk_input_size(self@time, "@time", 0, ),
      chk_input_size(self@date_phrase, "@date_phrase", 0, 1, 1),
      chk_input_size(self@sorting, "@sorting", 1, 1),
      chk_input_pattern(self@date, "@date", reg_date_value()),
      chk_input_pattern(self@time, "@time", reg_time())
    )
  }
)

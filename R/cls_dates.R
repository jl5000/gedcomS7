#' @include cls_validators.R
NULL

class_time <- S7::new_class("class_time",
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
                              hh_err <- mm_err <- ss_err <- ff_err <- NULL
                              if(length(self@hour) == 1 && floor(self@hour) != self@hour)
                                hh_err <- "Hour must be a whole number"
                              if(length(self@minute) == 1 && floor(self@minute) != self@minute)
                                mm_err <- "Minute must be a whole number"
                              if(length(self@second) == 1 && floor(self@second) != self@second)
                                ss_err <- "Second must be a whole number"
                              if(length(self@fraction) == 1 && floor(self@fraction) != self@fraction)
                                ff_err <- "Fraction must be a whole number"
                              c(
                                hh_err, mm_err, ss_err, ff_err,
                                chk_input_size(self@hour, "@hour", 1, 1, 0, 23),
                                chk_input_size(self@minute, "@minute", 1, 1, 0, 59),
                                chk_input_size(self@second, "@second", 0, 1, 0, 59),
                                chk_input_size(self@fraction, "@fraction", 0, 1, 1, 9),
                                chk_input_size(self@utc, "@utc", 1, 1)
                              )
                            })

class_date <- S7::new_class("class_date")

#' @export
class_date_exact <- S7::new_class("class_date_exact", parent = class_date,
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
                                dd_err <- mm_err <- yy_err <- NULL
                                if(length(self@day) == 1 && floor(self@day) != self@day)
                                  dd_err <- "Day must be a whole number"
                                if(length(self@month) == 1 && floor(self@month) != self@month)
                                  mm_err <- "Month must be a whole number"
                                if(length(self@year) == 1 && floor(self@year) != self@year)
                                  yy_err <- "Year must be a whole number"
                                c(
                                  dd_err, mm_err, yy_err,
                                  chk_input_size(self@day, "@day", 1, 1, 1, 31),
                                  chk_input_size(self@month, "@month", 1, 1, 1, 12),
                                  chk_input_size(self@year, "@year", 1, 1, 1),
                                  chk_input_date(self@year, self@month, self@day)
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
class_date_calendar <- S7::new_class("class_date_calendar", parent = class_date,
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
                               dd_err <- mm_err <- yy_err <- NULL
                               if(length(self@day) == 1 && floor(self@day) != self@day)
                                 dd_err <- "Day must be a whole number"
                               if(length(self@month) == 1 && floor(self@month) != self@month)
                                 mm_err <- "Month must be a whole number"
                               if(length(self@year) == 1 && floor(self@year) != self@year)
                                 yy_err <- "Year must be a whole number"
                               c(
                                 dd_err, mm_err, yy_err,
                                 chk_input_size(self@day, "@day", 0, 1, 1, 31),
                                 chk_input_size(self@month, "@month", 0, 1, 1, 12),
                                 chk_input_size(self@year, "@year", 1, 1, 1),
                                 chk_input_size(self@bce, "@bce", 1, 1),
                                 chk_input_date(self@year, self@month, self@day, self@bce)
                               )
                             }
)

#' @export
class_date_approx <- S7::new_class("class_date_approx", parent = class_date,
                                   properties = list(
                                     date = S7::new_property(S7::new_union(class_date_calendar, S7::class_character)),
                                     about = S7::new_property(S7::class_logical, default = TRUE),
                                     calc = S7::new_property(S7::class_logical, default = FALSE),
                                     est = S7::new_property(S7::class_logical, default = FALSE),
                                     
                                     as_val = S7::new_property(
                                       S7::class_character,
                                       getter = function(self){
                                         if(self@calc) {
                                           paste("CAL", self@date@as_val)
                                         } else if(self@est) {
                                           paste("EST", self@date@as_val)
                                         } else if (self@about) {
                                           paste("ABT", self@date@as_val)
                                         } 
                                       }               
                                     )
                                   ),
                                   validator = function(self){
                                     c(
                                       chk_input_size(self@date, "@date", 1, 1),
                                       chk_input_size(self@about, "@about", 1, 1),
                                       chk_input_size(self@calc, "@calc", 1, 1),
                                       chk_input_size(self@est, "@est", 1, 1)
                                     )
                                   }
)


#' @export
class_date_period <- S7::new_class("class_date_period", parent = class_date,
                               properties = list(
                                 start_date = S7::new_property(S7::new_union(NULL, class_date_calendar, S7::class_character)),
                                 end_date = S7::new_property(S7::new_union(NULL, class_date_calendar, S7::class_character)),
                                 
                                 as_val = S7::new_property(
                                   S7::class_character,
                                   getter = function(self){
                                     if (length(self@start_date) + length(self@end_date) == 2) {
                                       paste("FROM", self@start_date@as_val, 
                                             "TO", self@end_date@as_val)
                                     } else if (length(self@start_date) == 1) {
                                       paste("FROM", self@start_date@as_val)
                                     } else if (length(self@end_date) == 1) {
                                       paste("TO", self@end_date@as_val)
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
                                   chk_input_dates(self@start_date, self@end_date)
                                 )
                               }
)

#' @export
class_date_range <- S7::new_class("class_date_range", parent = class_date_period,
                              properties = list(
                                
                                as_val = S7::new_property(
                                  S7::class_character,
                                  getter = function(self){
                                    if (length(self@start_date) + length(self@end_date) == 2) {
                                      paste("BET", self@start_date@as_val, 
                                            "AND", self@end_date@as_val)
                                    } else if (length(self@start_date) == 1) {
                                      paste("AFT", self@start_date@as_val)
                                    } else if (length(self@end_date) == 1) {
                                      paste("BEF", self@end_date@as_val)
                                    } 
                                  }               
                                )
                              )
)


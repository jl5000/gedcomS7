#' @include cls_validators.R
NULL

class_time <- S7::new_class("class_time",
                            properties = list(
                              hour = S7::class_integer,
                              minute = S7::class_integer,
                              second = S7::class_integer,
                              fraction = S7::class_integer,
                              utc = S7::class_logical,
                              
                              as_val = S7::new_property(
                                S7::class_character,
                                getter = function(self){
                                  tim <- paste0(self@hour, ":", self@minute)
                                  if(length(self@second) == 1)
                                    tim <- paste0(tim, ":", self@second)
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
                                chk_input_size(self@hour, "@hour", 1, 1),
                                chk_input_size(self@minute, "@minute", 1, 1),
                                chk_input_size(self@second, "@second", 0, 1),
                                chk_input_size(self@fraction, "@fraction", 0, 1),
                                chk_input_size(self@utc, "@utc", 1, 1)
                              )
                            })

class_date <- S7::new_class("class_date")

#' @export
class_date_exact <- S7::new_class("class_date_exact", parent = class_date,
                              properties = list(
                                year = S7::class_integer,
                                month = S7::class_integer,
                                day = S7::class_integer,
                                
                                as_val = S7::new_property(
                                  S7::class_character,
                                  getter = function(self){
                                    paste(self@day, toupper(month.abb[self@month]), self@year)
                                  }),
                                
                                as_date = S7::new_property(
                                  S7::class_Date,
                                  getter = function(self){
                                    day <- self@day
                                    month <- self@month
                                    year <- self@year
                                    if(nchar(day) == 1) day <- paste0(0, day)
                                    if(nchar(month) == 1) month <- paste0(0, month)
                                    as.Date(paste(day, month, year), format = "%d %m %Y")
                                  })
                              ),
                              validator = function(self){
                                c(
                                  chk_input_size(self@day, "@day", 1, 1, 1, 2),
                                  chk_input_size(self@month, "@month", 1, 1, 1, 2),
                                  chk_input_size(self@year, "@year", 1, 1, 3, 4),
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
                               year = S7::class_integer,
                               month = S7::class_integer,
                               day = S7::class_integer,
                               year_is_bce = S7::new_property(S7::class_logical, default = FALSE),
                               year_is_dual = S7::new_property(S7::class_logical, default = FALSE),
                               
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
                                     if(length(self@month) + length(self@day) == 0 & self@year_is_bce)
                                       val <- paste(val, "BCE")
                                     if(length(self@month) == 1 & self@year_is_dual) {
                                       next_year <- self@year + 1
                                       # TODO: doesn't work with 3 digit years
                                       val <- paste0(val, "/", substr(next_year, 3, 4)) 
                                     }
                                   }
                                   trimws(val)
                                 }               
                               )
                             ),
                             validator = function(self){
                               c(
                                 chk_input_size(self@day, "@day", 0, 1, 1, 2),
                                 chk_input_size(self@month, "@month", 0, 1, 1, 2),
                                 chk_input_size(self@year, "@year", 0, 1, 3, 4),
                                 chk_input_size(self@year_is_bce, "@year_is_bce", 1, 1),
                                 chk_input_size(self@year_is_dual, "@year_is_dual", 1, 1),
                                 chk_input_date(self@year, self@month, self@day, 
                                                self@year_is_bce, self@year_is_dual)
                               )
                             }
)

#' @export
class_date_approx <- S7::new_class("class_date_approx", parent = class_date,
                                   properties = list(
                                     date = class_date_calendar,
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
                                 start_date = S7::new_property(S7::new_union(NULL, class_date_calendar)),
                                 end_date = S7::new_property(S7::new_union(NULL, class_date_calendar)),
                                 
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


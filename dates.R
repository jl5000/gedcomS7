
class_date_exact <- new_class("class_date_exact",
                              properties = list(
                                day = class_integer,
                                month = class_integer,
                                year = class_integer,
                                gedcom_value = new_property(
                                  class_character,
                                  getter = function(self){
                                    paste(self@day, toupper(month.abb[self@month]), self@year)
                                  }               
                                )
                              ),
                              validator = function(self){
                                c(
                                  chk_input_size(self@day, "@day", 1, 1, 1, 2),
                                  chk_input_size(self@month, "@month", 1, 1, 1, 2),
                                  chk_input_size(self@year, "@year", 1, 1, 3, 4)
                                  
                                )
                              }
)


class_date_calendar <- new_class("class_date_calendar",
                             properties = list(
                               day = class_integer,
                               month = class_integer,
                               year = class_integer,
                               year_is_bce = new_property(class_logical, default = FALSE),
                               year_is_dual = new_property(class_logical, default = FALSE),
                               gedcom_value = new_property(
                                 class_character,
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
                               if(length(self@year) + length(self@month) == 0){
                                 return("Date needs a month or year")
                               }
                             }
)


class_date_period <- new_class("class_date_period",
                               properties = list(
                                 start_date = new_property(new_union(NULL, class_date_calendar)),
                                 end_date = new_property(new_union(NULL, class_date_calendar)),
                                 gedcom_value = new_property(
                                   class_character,
                                   getter = function(self){
                                     if (length(self@start_date) + length(self@end_date) == 2) {
                                       paste("FROM", self@start_date@gedcom_value, 
                                             "TO", self@end_date@gedcom_value)
                                     } else if (length(self@start_date) == 1) {
                                       paste("FROM", self@start_date@gedcom_value)
                                     } else if (length(self@end_date) == 1) {
                                       paste("TO", self@end_date@gedcom_value)
                                     } 
                                   }               
                                 )
                               ),
                               validator = function(self){
                               
                               }
)

class_date_range <- new_class("class_date_range",
                              properties = list(
                                start_date = new_property(new_union(NULL, class_date_calendar)),
                                end_date = new_property(new_union(NULL, class_date_calendar)),
                                gedcom_value = new_property(
                                  class_character,
                                  getter = function(self){
                                    if (length(self@start_date) + length(self@end_date) == 2) {
                                      paste("BET", self@start_date@gedcom_value, 
                                            "AND", self@end_date@gedcom_value)
                                    } else if (length(self@start_date) == 1) {
                                      paste("AFT", self@start_date@gedcom_value)
                                    } else if (length(self@end_date) == 1) {
                                      paste("BEF", self@end_date@gedcom_value)
                                    } 
                                  }               
                                )
                              ),
                              validator = function(self){
                                
                              }
)

class_date_approx <- new_class("class_date_approx",
                               properties = list(
                                 date = class_date_calendar,
                                 about = new_property(class_logical, default = TRUE),
                                 calc = new_property(class_logical, default = FALSE),
                                 est = new_property(class_logical, default = FALSE),
                                 gedcom_value = new_property(
                                   class_character,
                                   getter = function(self){
                                     if(self@calc) {
                                       paste("CAL", self@date@gedcom_value)
                                     } else if(self@est) {
                                       paste("EST", self@date@gedcom_value)
                                     } else if (self@about) {
                                       paste("ABT", self@date@gedcom_value)
                                     } 
                                   }               
                                 )
                               ),
                               validator = function(self){
                                 
                               }
)

class_date_value <- new_class("class_date_value",
                              properties = list(
                                date = new_property(new_union(class_date_calendar,
                                                              class_date_period,
                                                              class_date_range,
                                                              class_date_approx))
                              ),
                              validator = function(self){
                                
                              }
)
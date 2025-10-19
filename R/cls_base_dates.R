
DateAny <- S7::new_class("DateAny", parent = GedcomS7class, abstract = TRUE)


#' Create a GEDCOM Exact Date object
#' 
#' @inheritParams prop_definitions
#' @returns An S7 object representing a GEDCOM Exact Date.
#' @export
#' @tests
#' expect_error(DateExact(), regexp = "@year has too few.*@month has too few.*@day has too few")
#' expect_error(DateExact(2001), regexp = "@month has too few.*@day has too few")
#' expect_error(DateExact(2001, 5), regexp = "@day has too few")
#' expect_error(DateExact(2001, 5, 32), regexp = "@day has a value which is too high")
#' expect_error(DateExact(2001, 2, 29), regexp = "Invalid date")
#' expect_equal(DateExact(2001, 5, 2)@GEDCOM_STRING, "2 MAY 2001")
#' expect_equal(DateExact(28, 7, 12)@GEDCOM_STRING, "12 JUL 28")
#' expect_equal(DateExact(28, 7, 12)@as_date, as.Date("28-07-12"))
DateExact <- S7::new_class(
  "DateExact", 
  parent = DateAny,
  properties = list(
    year = prop_whole(1, 1, 1),
    month = prop_whole(1, 1, 1, 12),
    day = prop_whole(1, 1, 1, 31),
    
    GEDCOM_STRING = S7::new_property(
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
#' @returns An S7 object representing a GEDCOM Exact Date for today.
#' @export
#' @tests
#' expect_equal(date_exact_current()@as_date, Sys.Date())
date_exact_current <- function(){
  DateExact(
    year = as.integer(format(Sys.Date(), "%Y")),
    month = as.integer(format(Sys.Date(), "%m")),
    day = as.integer(format(Sys.Date(), "%d"))
  )
}


#' Create a GEDCOM Calendar Date object
#' 
#' @inheritParams prop_definitions
#' @param year The year given as an integer (not 0). Negative years are interpreted
#' as Before the Common Era (BCE). If BCE, only year should be provided.
#' @param julian Whether the date is given in the Julian calendar. If not, it is
#' assumed to be in the Gregorian calendar.
#' 
#' @returns An S7 object representing a GEDCOM Calendar Date.
#' @export
#' @tests
#' expect_error(DateCalendar(), regexp = "@year has too few elements")
#' expect_error(DateCalendar(0), regexp = "@year cannot be zero")
#' expect_error(DateCalendar(2001, day = 15), regexp = "Day is defined without a month")
#' expect_error(DateCalendar(day = 5), regexp = "@year has too few elements")
#' expect_error(DateCalendar(month = 10), regexp = "@year has too few elements")
#' expect_error(DateCalendar(2010, 13, 3), regexp = "@month has a value which is too high")
#' expect_error(DateCalendar(2010, 1, 32), regexp = "@day has a value which is too high")
#' expect_error(DateCalendar(2001, 2, 30), regexp = "Invalid date")
#' expect_error(DateCalendar(-320, 5, 16), regexp = "BCE date must contain year only")
#' expect_equal(DateCalendar(2001, 5, 12)@GEDCOM_STRING, "12 MAY 2001")
#' expect_equal(DateCalendar(2001, 5, 12, TRUE)@GEDCOM_STRING, "JULIAN 12 MAY 2001")
#' expect_equal(DateCalendar(2004, 2, 29)@GEDCOM_STRING, "29 FEB 2004")
#' expect_equal(DateCalendar(2004, 8)@GEDCOM_STRING, "AUG 2004")
#' expect_equal(DateCalendar(2004, 8, julian = TRUE)@GEDCOM_STRING, "JULIAN AUG 2004")
#' expect_equal(DateCalendar(2012)@GEDCOM_STRING, "2012")
#' expect_equal(DateCalendar(-193)@GEDCOM_STRING, "193 BCE")
#' expect_equal(DateCalendar(-193, julian = TRUE)@GEDCOM_STRING, "JULIAN 193 BCE")
DateCalendar <- S7::new_class(
  "DateCalendar", 
  parent = DateAny,
  properties = list(
    year = prop_whole(1, 1),
    month = prop_whole(0, 1, 1, 12),
    day = prop_whole(0, 1, 1, 31),
    julian = prop_bool(FALSE),
    
    BCE = S7::new_property(
      S7::class_logical,
      getter = function(self){
        isTRUE(self@year < 0)
      }),
    
    GEDCOM_STRING = S7::new_property(
      S7::class_character,
      getter = function(self){
        val <- ""
        if (length(self@day) == 1) 
          val <- paste(val, self@day)
        if (length(self@month) == 1) 
          val <- paste(val, toupper(month.abb[self@month]))
        
        val <- paste(val, abs(self@year))
        if(self@BCE) val <- paste(val, "BCE")
        if(self@julian) val <- paste0("JULIAN", val)
            
        trimws(val)
      }               
    )
  ),
  validator = function(self){
    if(self@year == 0) return("@year cannot be zero")
    chk_input_date_cpts(self@year, self@month, self@day)
  }
)

#' Create a GEDCOM Approximate Date object
#' 
#' @param date_cal A Calendar date given either as a formatted GEDCOM string, or a
#' `DateCalendar` object.
#' @param about Whether the date is near to the date given.
#' @param calc Whether the date is calculated from other values.
#' @param est Whether the date is near to the date given, and is calculated from other values.
#' 
#' @returns An S7 object representing a GEDCOM Approximate Date.
#' @export
#' @tests
#' expect_error(DateApprox("hello"), regexp = "@date_cal is in an invalid format")
#' expect_equal(DateApprox(DateCalendar(2001, 5, 12), calc = TRUE)@GEDCOM_STRING, 
#'                               "CAL 12 MAY 2001")
#' expect_equal(DateApprox(DateCalendar(2004, 2, 29), about = TRUE)@GEDCOM_STRING, 
#'                               "ABT 29 FEB 2004")
#' expect_equal(DateApprox(DateCalendar(2004, 8), est = TRUE)@GEDCOM_STRING, 
#'                                "EST AUG 2004")
#' expect_equal(DateApprox(DateCalendar(2004, 8, julian = TRUE), est = TRUE)@GEDCOM_STRING, 
#'                                "EST JULIAN AUG 2004")
DateApprox <- S7::new_class(
  "DateApprox", 
  parent = DateAny,
  properties = list(
    date_cal = prop_char(1, 1, pattern = reg_date_calendar(), S7class_names = "DateCalendar"),
    about = prop_bool(default = TRUE),
    calc = prop_bool(default = FALSE),
    est = prop_bool(default = FALSE),
    
    GEDCOM_STRING = S7::new_property(
      S7::class_character,
      getter = function(self){
        if(self@calc) {
          paste("CAL", as_val(self@date_cal))
        } else if(self@est) {
          paste("EST", as_val(self@date_cal))
        } else if (self@about) {
          paste("ABT", as_val(self@date_cal))
        } 
      }               
    )
  )
)

DateSpan <- S7::new_class(
  "DateSpan",
  parent = DateAny,
  abstract = TRUE,
  properties = list(
    start_date = prop_char(0, 1, pattern = reg_date_calendar(), S7class_names = "DateCalendar"),
    end_date = prop_char(0, 1, pattern = reg_date_calendar(), S7class_names = "DateCalendar")
  ),
  validator = function(self){
    chk_input_dates(self@start_date, self@end_date)
  }
)

#' Create a GEDCOM Date Period object
#' 
#' @inheritParams prop_definitions
#' @returns An S7 object representing a GEDCOM Date Period.
#' @export
#' @tests
#' expect_equal(DatePeriod()@GEDCOM_STRING, "")
#' expect_error(DatePeriod(""), regexp = "@start_date is in an invalid format")
#' expect_error(DatePeriod(end_date = ""), regexp = "@end_date is in an invalid format")
#' expect_equal(DatePeriod("2 JUL 1989")@GEDCOM_STRING, "FROM 2 JUL 1989")
#' expect_equal(DatePeriod(end_date = "2 JUL 1989")@GEDCOM_STRING, "TO 2 JUL 1989")
#' expect_equal(
#'   DatePeriod(
#'     start_date = DateCalendar(1995, 6, 1)
#'   )@GEDCOM_STRING, "FROM 1 JUN 1995")
#' expect_equal(
#'   DatePeriod(
#'     end_date = DateCalendar(1995, 6, 1)
#'   )@GEDCOM_STRING, "TO 1 JUN 1995")
#' expect_equal(
#'   DatePeriod(
#'     start_date = DateCalendar(1990, 6, 1),
#'     end_date = DateCalendar(1995, 3)
#'   )@GEDCOM_STRING, "FROM 1 JUN 1990 TO MAR 1995")
#' expect_equal(
#'   DatePeriod(
#'     start_date = DateCalendar(1990, 6, 1, julian = TRUE),
#'     end_date = DateCalendar(1995, 3)
#'   )@GEDCOM_STRING, "FROM JULIAN 1 JUN 1990 TO GREGORIAN MAR 1995")
#' expect_error(
#'   DatePeriod(
#'     start_date = DateCalendar(1995, 6, 1),
#'     end_date = DateCalendar(1995, 6, 1)
#'   ), regexp = "Start date is the same as end date")
#' expect_error(
#'   DatePeriod(
#'     start_date = DateCalendar(2005, 6, 1),
#'     end_date = DateCalendar(1995, 6, 1)
#'   ), regexp = "Start date comes after end date")
#' expect_error(
#'   DatePeriod(
#'     start_date = DateCalendar(2005, 8, 1),
#'     end_date = DateCalendar(2005, 6, 1)
#'   ), regexp = "Start date comes after end date")
#' expect_error(
#'   DatePeriod(
#'     start_date = DateCalendar(2005, 8, 10),
#'     end_date = DateCalendar(2005, 8, 1)
#'   ), regexp = "Start date comes after end date")
DatePeriod <- S7::new_class(
  "DatePeriod", 
  parent = DateSpan,
  properties = list(
    GEDCOM_STRING = S7::new_property(
      S7::class_character,
      getter = function(self){
        if (length(self@start_date) + length(self@end_date) == 2) {
          start <- as_val(self@start_date)
          end <- as_val(self@end_date)
          if(grepl("JULIAN", start) != grepl("JULIAN", end)){
            start <- ifelse(grepl("JULIAN", start), start, paste("GREGORIAN", start))
            end <- ifelse(grepl("JULIAN", end), end, paste("GREGORIAN", end))
          }
          paste("FROM", start, "TO", end)
        } else if (length(self@start_date) == 1) {
          paste("FROM", as_val(self@start_date))
        } else if (length(self@end_date) == 1) {
          paste("TO", as_val(self@end_date))
        } else {
          ""
        }
      }               
    )
  )
)

#' Create a GEDCOM Date Range object
#' 
#' @inheritParams prop_definitions
#' @returns An S7 object representing a GEDCOM Date Range.
#' @export
#' @tests
#' expect_error(DateRange(), regexp = "has too few elements")
#' expect_error(DateRange(""), regexp = "@start_date is in an invalid format")
#' expect_error(DateRange(end_date = ""), regexp = "@end_date is in an invalid format")
#' expect_equal(DateRange("2 JUL 1989")@GEDCOM_STRING, "AFT 2 JUL 1989")
#' expect_equal(DateRange(end_date = "2 JUL 1989")@GEDCOM_STRING, "BEF 2 JUL 1989")
#' expect_equal(
#'   DateRange(
#'     start_date = DateCalendar(1995, 6, 1)
#'   )@GEDCOM_STRING, "AFT 1 JUN 1995")
#' expect_equal(
#'   DateRange(
#'     end_date = DateCalendar(1995, 6, 1)
#'   )@GEDCOM_STRING, "BEF 1 JUN 1995")
#' expect_equal(
#'   DateRange(
#'     start_date = DateCalendar(1990, 6, 1),
#'     end_date = DateCalendar(1995, 3)
#'   )@GEDCOM_STRING, "BET 1 JUN 1990 AND MAR 1995")
#' expect_equal(
#'   DateRange(
#'     start_date = DateCalendar(1990, 6, 1),
#'     end_date = DateCalendar(1995, 3, julian = TRUE)
#'   )@GEDCOM_STRING, "BET GREGORIAN 1 JUN 1990 AND JULIAN MAR 1995")
#' expect_equal(
#'   DateRange(
#'     start_date = DateCalendar(1990, 6, 1, julian = TRUE),
#'     end_date = DateCalendar(1995, 3, julian = TRUE)
#'   )@GEDCOM_STRING, "BET JULIAN 1 JUN 1990 AND JULIAN MAR 1995")
#' expect_error(
#'   DateRange(
#'    start_date = DateCalendar(1995, 6, 1),
#'     end_date = DateCalendar(1995, 6, 1)
#'   ), regexp = "Start date is the same as end date")
#' expect_error(
#'   DateRange(
#'     start_date = DateCalendar(2005, 6, 1),
#'     end_date = DateCalendar(1995, 6, 1)
#'   ), regexp = "Start date comes after end date")
#' expect_error(
#'   DateRange(
#'     start_date = DateCalendar(2005, 8, 1),
#'     end_date = DateCalendar(2005, 6, 1)
#'   ), regexp = "Start date comes after end date")
#' expect_error(
#'   DateRange(
#'     start_date = DateCalendar(2005, 8, 10),
#'     end_date = DateCalendar(2005, 8, 1)
#'   ), regexp = "Start date comes after end date")
DateRange <- S7::new_class(
  "DateRange", 
  parent = DateSpan,
  properties = list(
    
    GEDCOM_STRING = S7::new_property(
      S7::class_character,
      getter = function(self){
        if (length(self@start_date) + length(self@end_date) == 2) {
          start <- as_val(self@start_date)
          end <- as_val(self@end_date)
          if(grepl("JULIAN", start) != grepl("JULIAN", end)){
            start <- ifelse(grepl("JULIAN", start), start, paste("GREGORIAN", start))
            end <- ifelse(grepl("JULIAN", end), end, paste("GREGORIAN", end))
          }
          paste("BET", start, "AND", end)
        } else if (length(self@start_date) == 1) {
          paste("AFT", as_val(self@start_date))
        } else if (length(self@end_date) == 1) {
          paste("BEF", as_val(self@end_date))
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
#' @returns An S7 object representing a GEDCOM Date Value.
#' @export
#' @tests
#' expect_error(DateValue("FROM 2016", time = "12:34"), regexp = "A date period should not have a time defined")
#' expect_error(DateValue(DatePeriod(end_date = "1980"), time = Time(3,45,54,6765)), 
#'              regexp = "A date period should not have a time defined")
#' expect_error(DateValue(""), regexp = "A @date_phrase must be given if @date is ''")
#' expect_equal(DateValue(DateCalendar(2005, 1, 5))@GEDCOM_STRING, "5 JAN 2005")
#' expect_snapshot_value(DateValue("AFT 1990", date_phrase = "Maybe 1992")@GEDCOM, "json2")
#' expect_snapshot_value(DateValue("", date_phrase = "Phrase only", time = "02:24")@GEDCOM, "json2")
DateValue <- S7::new_class(
  "DateValue",
  parent = DateAny,
  properties = list(
    date = prop_char(1, 1, pattern = reg_date_value(),
                     S7class_names = c("DateCalendar", "DatePeriod",
                                       "DateRange", "DateApprox")),
    date_phrase = prop_char(0, 1, 1),
    time = prop_char(0, 1, pattern = reg_time(), S7class_names = "Time"),
    
    GEDCOM_STRING = S7::new_property(S7::class_character, 
                              getter = function(self) as_val(self@date)),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          as_ged(as_val(self@date), "DATE", 0),
          as_ged(as_val(self@time), "TIME", 1),
          as_ged(self@date_phrase, "PHRASE", 1)
        )
      })
  ),
  validator = function(self){
    #date_period has no time
    if(grepl("(FROM)|(TO)", as_val(self@date))){
      if(length(self@time) > 0)
        return("A date period should not have a time defined.")
    }
    
    chk_input_phrase(self@date_phrase, "@date_phrase",
                     as_val(self@date), "@date", "")
  }
)

#' Create a GEDCOM Sorting Date object
#' 
#' @param date The date given either as a formatted GEDCOM string, or a
#' `DateCalendar` object.
#' @inheritParams prop_definitions
#' 
#' @returns An S7 object representing a GEDCOM Sorting Date.
#' @export
#' @tests
#' expect_error(DateSorting(""), regexp = "@date is in an invalid format")
#' expect_error(DateSorting("FROM 2016"), regexp = "@date is in an invalid format")
#' expect_error(DateSorting(DatePeriod(end_date = "1980")), 
#'              regexp = "@date must be <character> or ")
#' expect_warning(DateSorting("JULIAN 2000"), 
#'                regexp = "It is recommended you use a sorting date in the Gregorian calendar")
#' expect_equal(DateSorting(DateCalendar(2005, 1, 5))@GEDCOM_STRING, "5 JAN 2005")
#' expect_snapshot_value(DateSorting("1990", date_phrase = "Maybe 1992")@GEDCOM, "json2")
DateSorting <- S7::new_class(
  "DateSorting",
  parent = DateAny,
  properties = list(
    date = prop_char(1, 1, pattern = reg_date_calendar(), S7class_names = "DateCalendar"),
    date_phrase = prop_char(0, 1, 1),
    time = prop_char(0, 1, pattern = reg_time(), S7class_names = "Time"),
    
    GEDCOM_STRING = S7::new_property(S7::class_character, 
                                getter = function(self) as_val(self@date)),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          as_ged(as_val(self@date), "SDATE", 0),
          as_ged(as_val(self@time), "TIME", 1),
          as_ged(self@date_phrase, "PHRASE", 1)
        )
      })
  ),
  validator = function(self){
    if(grepl("JULIAN", self@GEDCOM_STRING))
      warning("It is recommended you use a sorting date in the Gregorian calendar.")
    NULL
  }
)


parse_date_value <- function(lines, location = NULL, sorting = FALSE){
  if(sorting) tag <- "SDATE" else tag <- "DATE"
  date_val <- find_ged_values(lines, c(location, tag))
  date_phrase <- find_ged_values(lines, c(location, tag, "PHRASE"))
  
  if(length(date_val) + length(date_phrase) == 0) return(character()) 
  date_val <- chronify(date_val)
  
  if(sorting){
    dt <- DateSorting(date = date_val, date_phrase = date_phrase)
  } else {
    dt <- DateValue(date = date_val, date_phrase = date_phrase)
  }
  
  dt@time <- find_ged_values(lines, c(location, tag,"TIME"))

  dt
}


S7::method(summary, DateAny) <- function(object, ...){
  date_str <- object@GEDCOM_STRING
  
  if(S7::S7_inherits(object, DateSorting) || S7::S7_inherits(object, DateValue)){
    if(length(object@time) == 1) date_str <- paste(date_str, as_val(object@time))
    if(length(object@date_phrase) == 1) date_str <- sprintf("%s (%s)",
                                                       date_str, object@date_phrase)
  }
  
  to_console("Date:", date_str, 10)
}


AuditDate <- S7::new_class(
  "AuditDate",
  parent = GedcomS7class,
  abstract = TRUE,
  properties = list(
    date_exact = prop_char(1, 1, 
                           pattern = reg_date_exact(),
                           default = date_exact_current(),
                           S7class_names = "DateExact"),
    time = prop_char(0, 1, pattern = reg_time(), S7class_names = "Time")
  )
)

#' Create a creation date object
#' 
#' @inheritParams prop_definitions 
#' @returns An S7 object representing a GEDCOM CREATION_DATE.
#' @export
#' @tests
#' expect_error(CreationDate(date_exact = "1 JAM 2005"), regexp = "@date_exact is in an invalid format.")
#' expect_error(CreationDate(time = "123:34:45"), regexp = "@time is in an invalid format.")
#' expect_snapshot_value(CreationDate(date_exact = "1 JAN 2005")@GEDCOM, "json2")
#' expect_snapshot_value(CreationDate(date_exact = "1 JAN 2005",
#'                                           time = "11:04:56")@GEDCOM, "json2")
CreationDate <- S7::new_class(
  "CreationDate",
  parent = AuditDate,
  properties = list(
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          "0 CREA",
          sprintf("1 DATE %s", as_val(self@date_exact)),
          sprintf("2 TIME %s", as_val(self@time))
        )
      })
  )
)

#' Create a change date object
#' 
#' @inheritParams prop_definitions 
#' @returns An S7 object representing a GEDCOM CHANGE_DATE.
#' @export
#' @tests
#' expect_snapshot_value(ChangeDate(date = "1 JAN 2005",
#'                                         note_xrefs = "@23@",
#'                                         notes = c("note 1", "note 2"))@GEDCOM, "json2")
ChangeDate <- S7::new_class(
  "ChangeDate", 
  parent = AuditDate,
  properties = list(
    note_xrefs = prop_char(pattern = reg_xref(TRUE)),
    notes = prop_S7list("notes", Note),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          "0 CHAN",
          sprintf("1 DATE %s", as_val(self@date_exact)),
          sprintf("2 TIME %s", as_val(self@time)),
          notes_ged(self@notes, self@note_xrefs) |> level_up(1)
        )
        
      })
  )
)

parse_creation_date <- function(rec_lines){
  crea_date <- find_ged_values(rec_lines, c("CREA","DATE"))
  if(length(crea_date) == 0) return(NULL)
  
  CreationDate(
    date_exact = crea_date,
    time = find_ged_values(rec_lines, c("CREA","DATE","TIME"))
  )
}

parse_change_date <- function(rec_lines){
  change_date <- find_ged_values(rec_lines, c("CHAN","DATE"))
  if(length(change_date) == 0) return(NULL)
  
  ChangeDate(
    date_exact = change_date,
    time = find_ged_values(rec_lines, c("CHAN","DATE","TIME")),
    notes = parse_notes(rec_lines, "CHAN"),
    note_xrefs = find_ged_values(rec_lines, c("CHAN","SNOTE"))
  )
}

S7::method(summary, CreationDate) <- function(object, ...){
  exdent <- 15
  to_console_value_with_phrase("Created:", 
                               as_val(object@date_exact), 
                               as_val(object@time),
                               exdent,
                               "%s %s")
}

S7::method(summary, ChangeDate) <- function(object, ...){
  exdent <- 15
  to_console_value_with_phrase("Changed:", 
                               as_val(object@date_exact), 
                               as_val(object@time),
                               exdent,
                               "%s %s")
}

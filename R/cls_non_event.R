
#' Create a non-event object
#' 
#' @inheritParams prop_definitions 
#' @param event_type A code indicating the type of event that didn't happen. 
#' This must be taken from `val_event_types()`.
#' 
#' @returns An S7 object representing a GEDCOM NON_EVENT_STRUCTURE.
#' @export
#' @tests
#' expect_error(NonEvent("death"), regexp = "@event_type has an invalid value")
#' expect_error(NonEvent("DEAT", date_phrase = "Before the olympics"), 
#'              regexp = "@date_phrase requires a @date_period")
#' expect_error(NonEvent("DEAT", date_period = ""), 
#'              regexp = "A @date_phrase must be given if @date_period is ''")
#' expect_error(NonEvent("DEAT", date_period = DatePeriod()), 
#'              regexp = "A @date_phrase must be given if @date_period is ''")
#' expect_error(NonEvent("DEAT", date_period = "JAN 1984"), 
#'              regexp = "@date_period is in an invalid format")
#' expect_snapshot_value(NonEvent("IMMI", 
#'                                       date_period = DatePeriod("16 JUN 1980","1994"),
#'                                       date_phrase = "While parents alive",
#'                                       notes = "Note 1",
#'                                       citations = "@S98@")@GEDCOM, "json2")
NonEvent <- S7::new_class(
  "NonEvent",
  parent = GedcomS7class,
  properties = list(
    event_type = prop_char(1, 1, choices = val_event_types(FALSE)),
    date_period = prop_char(0, 1, pattern = reg_date_period(), S7class_names = "DatePeriod"),
    date_phrase = prop_char(0, 1, 1),
    note_xrefs = prop_char(pattern = reg_xref(TRUE)),
    notes = prop_S7list("notes", Note),
    citations = prop_S7list("citations", SourceCitation),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          as_ged(self@event_type, "NO", 0),
          as_ged(as_val(self@date_period), "DATE", 1) |> trimws(),
          as_ged(self@date_phrase, "PHRASE", 2),
          notes_ged(self@notes, self@note_xrefs, 1),
          as_ged(self@citations, 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_phrase(self@date_phrase, "@date_phrase",
                       as_val(self@date_period), "@date_period", ""),
      chk_input_parents(self@date_phrase, "@date_phrase", self@date_period, "@date_period")
    )
  }
)

parse_non_events <- function(rec_lines){
  none_lst <- find_ged_values(rec_lines, "NO", return_list = TRUE)

  lapply(none_lst, \(x){
    NonEvent(
      event_type = find_ged_values(x, "NO"),
      date_period = find_ged_values(x, c("NO","DATE")),
      date_phrase = find_ged_values(x, c("NO","DATE","PHRASE")),
      note_xrefs = find_ged_values(x, c("NO","SNOTE")),
      notes = parse_notes(x, "NO"),
      citations = parse_citations(x, "NO")
    )
  })
}

S7::method(summary, NonEvent) <- function(object, ...){
  exdent <- 15
  fact_type <- object@event_type
  fact_type <- names(val_fact_types(TRUE))[fact_type == val_fact_types(TRUE)]
  to_console("Non Event:", fact_type, exdent)
  to_console_value_with_phrase("Period:", 
                               as_val(object@date_period), object@date_phrase, 
                               exdent)
  cat("\n")
  to_console("Citations:", length(object@citations), exdent)
  to_console("Notes:", length(object@notes) + length(object@note_xrefs), exdent)
}

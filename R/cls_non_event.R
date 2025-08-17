
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
    event_type = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    c(
                                      chk_input_size(value, 1, 1),
                                      chk_input_choice(value, val_event_types(FALSE))
                                    )
                                  }),
    date_period = S7::new_property(S7::class_character | 
                                     S7::new_S3_class("gedcomS7::DatePeriod"),
                                   validator = function(value){
                                     c(
                                       chk_input_size(value, 0, 1),
                                       chk_input_pattern(value, reg_date_period())
                                     )
                                   }),
    date_phrase = S7::new_property(S7::class_character,
                                   validator = function(value){
                                     chk_input_size(value, 0, 1, 1)
                                   }),
    note_xrefs = prop_xref(),
    notes = prop_notes(),
    citations = S7::new_property(S7::class_list,
                                 getter = function(self) self@citations,
                                 setter = function(self, value){
                                   self@citations <- as.S7class_list(value, gedcomS7::SourceCitation)
                                   self
                                 },
                                 validator = function(value){
                                   for(inp in value) if(is.character(inp)) return(inp)
                                 }),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 NO %s", self@event_type),
          sprintf("1 DATE %s", obj_to_val(self@date_period)) |> trimws(),
          sprintf("2 PHRASE %s", self@date_phrase),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_phrase(self@date_phrase, "@date_phrase",
                       obj_to_val(self@date_period), "@date_period", ""),
      chk_input_parents(self@date_phrase, "@date_phrase", self@date_period, "@date_period")
    )
  }
)

parse_non_events <- function(rec_lines){
  none_lst <- find_ged_values(rec_lines, "NO", return_list = TRUE)
  if(length(none_lst) == 0) return(list())
  
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
                               obj_to_val(object@date_period), object@date_phrase, 
                               exdent)
  cat("\n")
  to_console("Citations:", length(object@citations), exdent)
  to_console("Notes:", length(object@notes) + length(object@note_xrefs), exdent)
}

#' @include cls_validators.R
NULL

#' Create a non-event object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM NON_EVENT_STRUCTURE.
#' @export
#' @include cls_date.R cls_note.R cls_citation.R
#' @tests
#' expect_error(class_non_event("death"), regexp = "@event_type has an invalid value")
#' expect_error(class_non_event("DEAT", date_phrase = "Before the olympics"), 
#'              regexp = "@date_phrase requires a @date_period")
#' expect_error(class_non_event("DEAT", date_period = "JAN 1984"), 
#'              regexp = "@date_period is in an invalid format")
#' expect_snapshot_value(class_non_event("IMMI", 
#'                                       date_period = class_date_period("16 JUN 1980","1994"),
#'                                       date_phrase = "While parents alive",
#'                                       notes = "Note 1",
#'                                       citations = "@S98@")@as_ged, "json2")
class_non_event <- S7::new_class(
  "class_non_event",
  package = "gedcomS7",
  properties = list(
    event_type = S7::class_character,
    date_period = S7::class_character | class_date_period,
    date_phrase = S7::class_character,
    note_xrefs = S7::class_character,
    notes = S7::class_list | class_note | S7::class_character,
    citations = S7::class_list | class_citation | S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 NO %s", self@event_type),
          sprintf("1 DATE %s", obj_to_val(self@date_period)) |> trimws(),
          sprintf("2 PHRASE %s", self@date_phrase),
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@event_type, "@event_type", 1, 1),
      chk_input_choice(self@event_type, "@event_type", val_event_types(FALSE)),
      chk_input_size(self@date_period, "@date_period", 0, 1),
      chk_input_pattern(self@date_period, "@date_period", reg_date_period()),
      chk_input_size(self@date_phrase, "@date_phrase", 0, 1, 1),
      chk_input_parents(self@date_phrase, "@date_phrase", self@date_period, "@date_period"),
      chk_input_pattern(self@note_xrefs, "@note_xrefs", reg_xref(TRUE)),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+"),
      chk_input_S7classes(self@citations, "@citations", class_citation, reg_xref(TRUE))
    )
  }
)

extract_non_events <- function(rec_lines, location = NULL){
  none_lst <- find_ged_values(rec_lines, c(location, "NO"), return_list = TRUE)
  if(length(none_lst) == 0) return(list())
  
  lapply(none_lst, \(x){
    class_association(
      event_type = find_ged_values(x, "NO"),
      date_period = find_ged_values(x, c("NO","DATE")),
      date_phrase = find_ged_values(x, c("NO","DATE","PHRASE")),
      note_xrefs = find_ged_values(x, c("NO","SNOTE")),
      notes = extract_notes(x, "NO"),
      citations = extract_citations(x, "NO")
    )
  })
}
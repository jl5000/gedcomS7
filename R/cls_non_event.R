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
    event_type = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    c(
                                      chk_input_size(value, 1, 1),
                                      chk_input_choice(value, val_event_types(FALSE))
                                    )
                                  }),
    date_period = S7::new_property(S7::class_character | class_date_period,
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
    note_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    notes = S7::new_property(S7::class_list | class_note | S7::class_character,
                             validator = function(value){
                               chk_input_S7classes(value, class_note, ".+")
                             }),
    citations = S7::new_property(S7::class_list | class_citation | S7::class_character,
                                 validator = function(value){
                                   chk_input_S7classes(value, class_citation, reg_xref(TRUE))
                                 }),
    
    as_ged = S7::new_property(
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
    chk_input_parents(self@date_phrase, "@date_phrase", self@date_period, "@date_period")
  }
)

extract_non_events <- function(rec_lines){
  none_lst <- find_ged_values(rec_lines, "NO", return_list = TRUE)
  if(length(none_lst) == 0) return(list())
  
  lapply(none_lst, \(x){
    class_non_event(
      event_type = find_ged_values(x, "NO"),
      date_period = find_ged_values(x, c("NO","DATE")),
      date_phrase = find_ged_values(x, c("NO","DATE","PHRASE")),
      note_xrefs = find_ged_values(x, c("NO","SNOTE")),
      notes = extract_notes(x, "NO"),
      citations = extract_citations(x, "NO")
    )
  })
}
#' @include cls_validators.R
NULL

#' @export
#' @include cls_date.R cls_note.R cls_citation.R
class_non_event <- S7::new_class(
  "class_non_event",
  package = "gedcomS7",
  properties = list(
    event = S7::class_character,
    date_period = S7::class_character | class_date_period,
    date_phrase = S7::class_character,
    note_uids = S7::class_character,
    notes = S7::class_list | class_note | S7::class_character,
    citations = S7::class_list | class_citation | S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 NO %s", self@event),
          sprintf("1 DATE %s", self@date_period),
          sprintf("2 PHRASE %s", self@date_phrase),
          sprintf("1 SNOTE %s", self@note_uids),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@event, "@event", 1, 1),
      chk_input_size(self@date_period, "@date_period", 0, 1),
      chk_input_size(self@date_phrase, "@date_phrase", 0, 1, 1),
      chk_input_pattern(self@date_period, "@date_period", reg_date_period()),
      chk_input_pattern(self@note_uids, "@note_uids", reg_uuid(TRUE)),
      chk_input_choice(self@event, "@event", val_event_types(FALSE)),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+"),
      chk_input_S7classes(self@citations, "@citations", class_citation, reg_uuid(TRUE))
    )
  }
)

#' @include cls_validators.R
NULL

#' @export
#' @include cls_note.R
class_repository_citation <- S7::new_class(
  "class_repository_citation",
  package = "gedcomS7",
  properties = list(
    repo_uid = S7::class_character,
    notes = S7::class_list | S7::class_character,
    note_uids = S7::class_character,
    source_call_number = S7::class_character,
    # media = S7::class_character, TODO: too much nesting
    # media_phrase = S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 REPO %s", self@repo_uid),
          sprintf("1 SNOTE %s", self@note_uids),
          lst_to_ged(self@notes) |> increase_level(by = 1),
          sprintf("1 CALN %s", self@source_call_number)
          # sprintf("2 MEDI %s", self@media),
          # sprintf("3 PHRASE %s", self@media_phrase)
        )
      })
  ),
  
  validator = function(self){
    c(
      chk_input_size(self@repo_uid, "@repo_uid", 1, 1),
      chk_input_pattern(self@note_uids, "@note_uids", reg_uuid(TRUE)),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+"),
      chk_input_pattern(self@repo_uid, "@repo_uid", reg_uuid(TRUE)),
      chk_input_size(self@source_call_number, "@source_call_number", min_val = 1)
    )
  }
)

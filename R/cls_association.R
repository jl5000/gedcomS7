#' @include cls_validators.R
NULL

#' @export
#' @include cls_note.R cls_citation.R
class_association <- S7::new_class(
  "class_association",
  package = "gedcomS7",
  properties = list(
    indi_uid = S7::class_character,
    indi_phrase = S7::class_character,
    relation_is = S7::class_character,
    relation_phrase = S7::class_character,
    note_uids = S7::class_character,
    notes = S7::class_list | S7::class_character,
    citations = S7::class_list,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        if(length(self@indi_uid) == 0)
          indi_uid <- "@VOID@"
        c(
          sprintf("0 ASSO %s", indi_uid),
          sprintf("1 PHRASE %s", self@indi_phrase),
          sprintf("1 ROLE %s", self@relation_is),
          sprintf("2 PHRASE %s", self@relation_phrase),
          sprintf("1 SNOTE %s", self@note_uids),
          lst_to_ged(self@notes) |> increase_level(by = 1),
          lst_to_ged(self@citations) |> increase_level(by = 1)
        )
      })
  ),
  
  validator = function(self){
    c(
      #TODO: Deal with @VOID@
      chk_input_size(self@indi_uid, "@indi_uid", 1, 1),
      chk_input_pattern(self@indi_uid, "@indi_uid", reg_uuid(TRUE)),
      chk_input_size(self@indi_phrase, "@indi_phrase", 0, 1, 1),
      chk_input_size(self@relation_is, "@relation_is", 1, 1),
      chk_input_choice(self@relation_is, "@relation_is", val_roles()),
      chk_input_size(self@relation_phrase, "@relation_phrase", 0, 1, 1),
      chk_input_pattern(self@note_uids, "@note_uids", reg_uuid(TRUE)),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+"),
      chk_input_S7classes(self@citations, "@citations", class_citation)
    )
  }
)

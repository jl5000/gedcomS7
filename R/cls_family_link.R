#' @include cls_validators.R
NULL


#' @export
#' @include cls_note.R
class_spouse_family_link <- S7::new_class(
  "class_spouse_family_link",
  package = "gedcomS7",
  properties = list(
    fam_uid = S7::class_character,
    note_uids = S7::class_character,
    notes = S7::class_list,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 FAMS %s", self@fam_uid),
          sprintf("1 SNOTE %s", self@note_uids),
          lst_to_ged(self@notes) |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@fam_uid, "@fam_uid", 1, 1),
      chk_input_pattern(self@fam_uid, "@fam_uid", reg_uuid(TRUE)),
      chk_input_pattern(self@note_uids, "@note_uids", reg_uuid(TRUE)),
      chk_input_S7classes(self@notes, "@notes", class_note)
    )
  }
)

#' @export
class_child_family_link <- S7::new_class(
  "class_child_family_link", 
  package = "gedcomS7",
  parent = class_spouse_family_link,
  properties = list(
    pedigree = S7::new_property(S7::class_character,
                                getter = function(self) "BIRTH"),
    pedigree_phrase = S7::class_character,
    certainty = S7::class_character,
    certainty_phrase = S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 FAMS %s", self@fam_uid),
          sprintf("1 PEDI %s", self@pedigree),
          sprintf("2 PHRASE %s", self@pedigree_phrase),
          sprintf("1 STAT %s", self@certainty),
          sprintf("2 PHRASE %s", self@certainty_phrase),
          sprintf("1 SNOTE %s", self@note_uids),
          lst_to_ged(self@notes) |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    #TODO: check phrases without parent
    c(
      chk_input_size(self@pedigree, "@pedigree", 0, 1),
      chk_input_choice(self@pedigree, "@pedigree", val_pedigree_linkage_types()),
      chk_input_size(self@pedigree_phrase, "@pedigree_phrase", 0, 1, 1),
      chk_input_size(self@certainty, "@certainty", 0, 1),
      chk_input_choice(self@certainty, "@certainty", val_pedigree_certainty()),
      chk_input_size(self@certainty_phrase, "@certainty_phrase", 0, 1, 1)
    )
  }
)


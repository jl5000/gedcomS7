#' @include cls_validators.R
NULL

#' Create an association object
#' 
#' @inheritParams prop_definitions
#' @return An S7 object representing a GEDCOM ASSOCIATION_STRUCTURE.
#' @export
#' @include cls_note.R cls_citation.R
#' @tests
#' 
class_association <- S7::new_class(
  "class_association",
  package = "gedcomS7",
  properties = list(
    indi_xref = S7::new_property(S7::class_character, default = "@VOID@"),
    indi_phrase = S7::class_character,
    relation_is = S7::class_character,
    relation_phrase = S7::class_character,
    note_xrefs = S7::class_character,
    notes = S7::class_list | class_note | S7::class_character,
    citations = S7::class_list | class_citation | S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 ASSO %s", indi_xref),
          sprintf("1 PHRASE %s", self@indi_phrase),
          sprintf("1 ROLE %s", self@relation_is),
          sprintf("2 PHRASE %s", self@relation_phrase),
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1)
        )
      })
  ),
  
  validator = function(self){
    c(
      chk_input_size(self@indi_xref, "@indi_xref", 1, 1),
      chk_input_pattern(self@indi_xref, "@indi_xref", reg_xref(TRUE)),
      chk_input_size(self@indi_phrase, "@indi_phrase", 0, 1, 1),
      chk_input_size(self@relation_is, "@relation_is", 1, 1),
      chk_input_choice(self@relation_is, "@relation_is", val_roles()),
      chk_input_size(self@relation_phrase, "@relation_phrase", 0, 1, 1),
      chk_input_pattern(self@note_xrefs, "@note_xrefs", reg_xref(TRUE)),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+"),
      chk_input_S7classes(self@citations, "@citations", class_citation, reg_xref(TRUE))
    )
  }
)

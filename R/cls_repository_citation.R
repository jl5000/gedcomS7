#' @include cls_validators.R
NULL


#' Create a repository citation object
#' 
#' @inheritParams prop_definitions
#' @return An S7 object representing a GEDCOM SOURCE_REPOSITORY_CITATION.
#' @export
#' @include cls_note.R
#' @tests
#' expect_snapshot_value(class_repository_citation()@as_ged, "json2")
#' expect_snapshot_value(class_repository_citation(notes = "Local library",
#'                                                 call_numbers = c("ABC","123"))@as_ged, "json2")
class_repository_citation <- S7::new_class(
  "class_repository_citation",
  package = "gedcomS7",
  properties = list(
    repo_xref = S7::new_property(S7::class_character, default = "@VOID@"),
    notes = S7::class_list | class_note | S7::class_character,
    note_xrefs = S7::class_character,
    call_numbers = S7::class_character,
    # media = S7::class_character, TODO: too much nesting
    # media_phrase = S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 REPO %s", self@repo_xref),
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          sprintf("1 CALN %s", self@call_numbers)
          # sprintf("2 MEDI %s", self@media),
          # sprintf("3 PHRASE %s", self@media_phrase)
        )
      })
  ),
  
  validator = function(self){
    c(
      chk_input_size(self@repo_xref, "@repo_xref", 1, 1),
      chk_input_pattern(self@note_xrefs, "@note_xrefs", reg_xref(TRUE)),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+"),
      chk_input_pattern(self@repo_xref, "@repo_xref", reg_xref(TRUE)),
      chk_input_size(self@call_numbers, "@call_numbers", min_val = 1)
    )
  }
)

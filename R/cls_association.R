#' @include cls_validators.R
NULL

#' Create an association object
#' 
#' @inheritParams prop_definitions
#' @return An S7 object representing a GEDCOM ASSOCIATION_STRUCTURE.
#' @export
#' @include cls_note.R cls_citation.R
#' @tests
#' expect_snapshot_value(class_association(relation_is = "FATH")@as_ged, "json2")
#' expect_error(class_association(indi_phrase = "someone", relation_is = "CHILD"),
#'              regexp = "@relation_is has an invalid value")
#' expect_snapshot_value(class_association(indi_phrase = "someone",
#'                                         relation_is = "FATH")@as_ged, "json2")
#' expect_snapshot_value(class_association(indi_xref = "@SME@", indi_phrase = "someone",
#'                                         relation_is = "FATH", relation_phrase = "step-father",
#'                                         note_xrefs = c("@352@","@564@"),
#'                                         notes = "This is a note",
#'                                         citations = class_citation("@S45@",
#'                                                                    where = "Page 2"))@as_ged, "json2") 
class_association <- S7::new_class(
  "class_association",
  package = "gedcomS7",
  properties = list(
    indi_xref = S7::new_property(S7::class_character, default = "@VOID@",
                                 validator = function(value){
                                   c(
                                     chk_input_size(value, 1, 1),
                                     chk_input_pattern(value, reg_xref(TRUE))
                                   )
                                 }),
    indi_phrase = S7::new_property(S7::class_character,
                                   validator = function(value){
                                     chk_input_size(value, 0, 1, 1)
                                   }),
    relation_is = S7::new_property(S7::class_character,
                                   validator = function(value){
                                     c(
                                       chk_input_size(value, 1, 1),
                                       chk_input_choice(value, val_roles())
                                     )
                                   }),
    relation_phrase = S7::new_property(S7::class_character,
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
          sprintf("0 ASSO %s", self@indi_xref),
          sprintf("1 PHRASE %s", self@indi_phrase),
          sprintf("1 ROLE %s", self@relation_is),
          sprintf("2 PHRASE %s", self@relation_phrase),
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1)
        )
      })
  )
)


extract_associations <- function(rec_lines, location = NULL){
  asso_lst <- find_ged_values(rec_lines, c(location, "ASSO"), return_list = TRUE)
  if(length(asso_lst) == 0) return(list())
  
  lapply(asso_lst, \(x){
    class_association(
      indi_xref = find_ged_values(x, "ASSO"),
      indi_phrase = find_ged_values(x, c("ASSO","PHRASE")),
      relation_is = find_ged_values(x, c("ASSO","ROLE")),
      relation_phrase = find_ged_values(x, c("ASSO","ROLE","PHRASE")),
      note_xrefs = find_ged_values(x, c("ASSO","SNOTE")),
      notes = extract_notes(x, "ASSO"),
      citations = extract_citations(x, "ASSO")
    )
  })
}

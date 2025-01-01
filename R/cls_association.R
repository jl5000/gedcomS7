
#' Create an association object
#' 
#' @inheritParams prop_definitions
#' @param indi_xref The cross-reference identifier of an individual record. If the individual
#' does not have a record, then this can be left blank and the value "@VOID@" will be used. However,
#' you should define an @indi_phrase.
#' @param indi_phrase Textual information that cannot be expressed in the @indi_xref.
#' @param relation_is The nature of the association. This must be a value from `val_roles()`.
#' If a value of "OTHER" is used, a @relation_phrase must be given.
#' @param relation_phrase Textual information that cannot be expressed in the relation.
#' 
#' @returns An S7 object representing a GEDCOM ASSOCIATION_STRUCTURE.
#' @export
#' @tests
#' expect_snapshot_value(Association(relation_is = "FATH")@c_as_ged, "json2")
#' expect_error(Association(indi_phrase = "someone", relation_is = "CHILD"),
#'              regexp = "@relation_is has an invalid value")
#' expect_error(Association(indi_phrase = "someone", relation_is = "OTHER"),
#'              regexp = "A @relation_phrase must be given")
#' expect_snapshot_value(Association(indi_phrase = "someone",
#'                                         relation_is = "FATH")@c_as_ged, "json2")
#' expect_snapshot_value(Association(indi_xref = "@SME@", indi_phrase = "someone",
#'                                         relation_is = "FATH", relation_phrase = "step-father",
#'                                         note_xrefs = c("@352@","@564@"),
#'                                         notes = "This is a note",
#'                                         citations = SourceCitation("@S45@",
#'                                                                    where = "Page 2"))@c_as_ged, "json2") 
Association <- S7::new_class(
  "Association",
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
    notes = S7::new_property(S7::class_list | 
                               S7::new_S3_class("gedcomS7::Note") | 
                               S7::class_character,
                             validator = function(value){
                               chk_input_S7classes(value, Note, ".+")
                             }),
    citations = S7::new_property(S7::class_list | 
                                   S7::new_S3_class("gedcomS7::SourceCitation") | 
                                   S7::class_character,
                                 validator = function(value){
                                   chk_input_S7classes(value, SourceCitation, reg_xref(TRUE))
                                 }),
    
    c_as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 ASSO %s", self@indi_xref),
          sprintf("1 PHRASE %s", self@indi_phrase),
          sprintf("1 ROLE %s", self@relation_is),
          sprintf("2 PHRASE %s", self@relation_phrase),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    if(self@relation_is == "OTHER" && length(self@relation_phrase) == 0)
      "A @relation_phrase must be given if @relation_is is 'OTHER'"
  }
)


parse_associations <- function(rec_lines, location = NULL){
  asso_lst <- find_ged_values(rec_lines, c(location, "ASSO"), return_list = TRUE)
  if(length(asso_lst) == 0) return(list())
  
  lapply(asso_lst, \(x){
    Association(
      indi_xref = find_ged_values(x, "ASSO"),
      indi_phrase = find_ged_values(x, c("ASSO","PHRASE")),
      relation_is = find_ged_values(x, c("ASSO","ROLE")),
      relation_phrase = find_ged_values(x, c("ASSO","ROLE","PHRASE")),
      note_xrefs = find_ged_values(x, c("ASSO","SNOTE")),
      notes = parse_notes(x, "ASSO"),
      citations = parse_citations(x, "ASSO")
    )
  })
}

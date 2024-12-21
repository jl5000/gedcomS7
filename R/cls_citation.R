
#' Create a source citation object
#' 
#' @inheritParams prop_definitions
#' @param sour_xref The cross-reference identifier of a source record. If the source
#' does not have a record, then this can be left blank and the value "@VOID@" will be used. However,
#' you should describe the source in @where.
#' @param where A specific location within the information referenced. For a published work, this could
#' include the volume of a multi-volume work and the page number or numbers. For a
#' periodical, it could include volume, issue, and page numbers. For a newspaper, it could
#' include a date, page number, and column number. For an unpublished source or micro‐
#' filmed works, this could be a film or sheet number, page number, or frame number. A
#' census record might have an enumerating district, page number, line number, dwelling
#' number, and family number.
#' It is recommended that the data in this field be formatted comma-separated with label:
#'   value pairs
#' @param fact_phrase Textual information that cannot be expressed in the fact type.
#' @param role What role this person played in this fact.
#' @param role_phrase Textual information that cannot be expressed in the role.
#' @param certainty An enumerated value indicating the credibility of a
#' piece of information, based on its supporting evidence. Some systems use this feature to
#' rank multiple conflicting opinions for display of most likely information first. It is not
#' intended to eliminate the receivers’ need to evaluate the evidence for themselves.
#' "0" = unreliable/estimated data
#' "1" = Questionable reliability of evidence 
#' "2" = Secondary evidence, data officially recorded sometime after event
#' "3" = Direct and primary evidence used, or by dominance of the evidence
#' 
#' @return An S7 object representing a GEDCOM SOURCE_CITATION.
#' @export
#' @tests
#' expect_snapshot_value(SourceCitation()@c_as_ged, "json2")
#' expect_error(SourceCitation("@1@",
#'                             fact_phrase = "phrase"),
#'              regexp = "@fact_phrase requires a @fact_type")
#' expect_error(SourceCitation("@1@",
#'                             role = "HUSB"),
#'              regexp = "@role requires a @fact_type")
#' expect_error(SourceCitation("@1@",
#'                             fact_type = "BIRT", role_phrase = "phrase"),
#'              regexp = "@role_phrase requires a @role")
#' expect_error(SourceCitation("@1@",
#'                             certainty = "4"),
#'              regexp = "@certainty has an invalid value")
#' expect_error(SourceCitation("@1@",
#'                             notes = ""),
#'              regexp = "@notes is in an invalid format")
#' expect_error(SourceCitation("@1@",
#'                             fact_type = "birth"),
#'              regexp = "@fact_type has an invalid value")             
#' expect_snapshot_value(SourceCitation("@1@",
#'                                      where = "page 2",
#'                                      date = "2 JUN 2006",
#'                                      source_text = c("verbatim","text"),
#'                                      fact_type = "BIRT",
#'                                      fact_phrase = "Parish births",
#'                                      role = "HUSB",
#'                                      role_phrase = "phrase",
#'                                      certainty = "3",
#'                                      media_links = MediaLink("@34E@"),
#'                                      note_xrefs = c("@WER@",
#'                                                    "@4334@"),
#'                                      notes = c("these are","some notes"))@c_as_ged, "json2")
SourceCitation <- S7::new_class(
  "SourceCitation",
  properties = list(
    sour_xref = S7::new_property(S7::class_character, default = "@VOID@",
                                 validator = function(value){
                                   c(
                                     chk_input_size(value, 1, 1),
                                     chk_input_pattern(value, reg_xref(TRUE))
                                   )
                                 }),
    where = S7::new_property(S7::class_character,
                             validator = function(value){
                               chk_input_size(value, 0, 1, 1)
                             }),
    date = S7::new_property(S7::class_character | 
                              S7::new_S3_class("gedcomS7::DateValue"),
                            validator = function(value){
                              c(
                                chk_input_size(value, 0, 1),
                                chk_input_pattern(value, reg_date_value())
                              )
                            }),
    source_text = S7::new_property(S7::class_list | 
                                     S7::new_S3_class("gedcomS7::TranslationText") | 
                                     S7::class_character,
                                   validator = function(value){
                                     chk_input_S7classes(value, TranslationText, ".+")
                                   }),
    fact_type = S7::new_property(S7::class_character,
                                 validator = function(value){
                                   c(
                                     chk_input_size(value, 0, 1),
                                     chk_input_choice(value, val_fact_types())
                                   )
                                 }),
    fact_phrase = S7::new_property(S7::class_character,
                                   validator = function(value){
                                     chk_input_size(value, 0, 1, 1)
                                   }),
    role = S7::new_property(S7::class_character,
                            validator = function(value){
                              c(
                                chk_input_size(value, 0, 1),
                                chk_input_choice(value, val_roles())
                              )
                            }),
    role_phrase = S7::new_property(S7::class_character,
                                   validator = function(value){
                                     chk_input_size(value, 0, 1, 1)
                                   }),
    certainty = S7::new_property(S7::class_character,
                                 validator = function(value){
                                   c(
                                     chk_input_size(value, 0, 1),
                                     chk_input_choice(value, val_certainty())
                                   )
                                 }),
    media_links = S7::new_property(S7::class_list | 
                                     S7::new_S3_class("gedcomS7::MediaLink") | 
                                     S7::class_character,
                                   validator = function(value){
                                     chk_input_S7classes(value, MediaLink, reg_xref(TRUE))
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
    
    c_as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 SOUR %s", self@sour_xref),
          sprintf("1 PAGE %s", self@where),
          rep("1 DATA", length(self@date) + 
                length(self@source_text) > 0),
          obj_to_ged(self@date, "DATE") |> increase_level(by = 2),
          obj_to_ged(self@source_text, "TEXT") |> increase_level(by = 2) |> 
            gsub(pattern = "(^\\d) TRAN ", replacement = "\\1 TEXT "),
          sprintf("1 EVEN %s", self@fact_type),
          sprintf("2 PHRASE %s", self@fact_phrase),
          sprintf("2 ROLE %s", self@role),
          sprintf("3 PHRASE %s", self@role_phrase),
          sprintf("1 QUAY %s", self@certainty),
          obj_to_ged(self@media_links, "OBJE") |> increase_level(by = 1),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_xrefs)
        ) 
      })
  ),
  
  validator = function(self){
    c(
      chk_input_parents(self@fact_phrase, "@fact_phrase", self@fact_type, "@fact_type"),
      chk_input_parents(self@role, "@role", self@fact_type, "@fact_type"),
      chk_input_parents(self@role_phrase, "@role_phrase", self@role, "@role")
    )
  }
  
)

parse_citations <- function(lines, location = NULL){
  
  sour_lst <- find_ged_values(lines, c(location, "SOUR"), return_list = TRUE)
  if(length(sour_lst) == 0) return(list())
  
  lapply(sour_lst, \(x){
    SourceCitation(
      sour_xref = find_ged_values(x, "SOUR"),
      where = find_ged_values(x, c("SOUR","PAGE")),
      date = parse_date_value(x, c("SOUR","DATA")),
      source_text = parse_translations(x, c("SOUR","DATA")),
      fact_type = find_ged_values(x, c("SOUR","EVEN")),
      fact_phrase = find_ged_values(x, c("SOUR","EVEN","PHRASE")),
      role = find_ged_values(x, c("SOUR","EVEN","ROLE")),
      role_phrase = find_ged_values(x, c("SOUR","EVEN","ROLE","PHRASE")),
      certainty = find_ged_values(x, c("SOUR","QUAY")),
      media_links = parse_media_links(x, "SOUR"),
      notes = parse_notes(x, "SOUR"),
      note_xrefs = find_ged_values(x, c("SOUR","SNOTE"))
    )
  })
  
}

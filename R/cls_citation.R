#' @include cls_validators.R
NULL

#' Create a source citation object
#' 
#' @inheritParams prop_definitions
#' @return An S7 object representing a GEDCOM SOURCE_CITATION.
#' @export
#' @include cls_date.R cls_translation.R cls_media_link.R cls_note.R
#' @tests
#' expect_snapshot_value(class_citation()@as_ged, "json2")
#' expect_error(class_citation("@1@",
#'                             fact_phrase = "phrase"),
#'              regexp = "@fact_phrase requires a @fact_type")
#' expect_error(class_citation("@1@",
#'                             role = "HUSB"),
#'              regexp = "@role requires a @fact_type")
#' expect_error(class_citation("@1@",
#'                             fact_type = "BIRT", role_phrase = "phrase"),
#'              regexp = "@role_phrase requires a @role")
#' expect_error(class_citation("@1@",
#'                             certainty = "4"),
#'              regexp = "@certainty has an invalid value")
#' expect_error(class_citation("@1@",
#'                             notes = ""),
#'              regexp = "@notes is in an invalid format")
#' expect_error(class_citation("@1@",
#'                             fact_type = "birth"),
#'              regexp = "@fact_type has an invalid value")             
#' expect_snapshot_value(class_citation("@1@",
#'                                      where = "page 2",
#'                                      date = "2 JUN 2006",
#'                                      source_text = c("verbatim","text"),
#'                                      fact_type = "BIRT",
#'                                      fact_phrase = "Parish births",
#'                                      role = "HUSB",
#'                                      role_phrase = "phrase",
#'                                      certainty = "3",
#'                                      media_links = class_media_link("@34E@"),
#'                                      note_xrefs = c("@WER@",
#'                                                    "@4334@"),
#'                                      notes = c("these are","some notes"))@as_ged, "json2")
class_citation <- S7::new_class(
  "class_citation",
  package = "gedcomS7",
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
    date = S7::new_property(S7::class_character | class_date_value,
                            validator = function(value){
                              c(
                                chk_input_size(value, 0, 1),
                                chk_input_pattern(value, reg_date_value())
                              )
                            }),
    source_text = S7::new_property(S7::class_list | class_translation_txt | S7::class_character,
                                   validator = function(value){
                                     chk_input_S7classes(value, class_translation_txt, ".+")
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
    media_links = S7::new_property(S7::class_list | class_media_link | S7::class_character,
                                   validator = function(value){
                                     chk_input_S7classes(value, class_media_link, reg_xref(TRUE))
                                   }),
    note_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    notes = S7::new_property(S7::class_list | class_note | S7::class_character,
                             validator = function(value){
                               chk_input_S7classes(value, class_note, ".+")
                             }),
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 SOUR %s", self@sour_xref),
          sprintf("1 PAGE %s", self@where),
          rep("1 DATA", length(self@date) + 
                length(self@source_text) > 0),
          obj_to_ged(self@date, "DATE") |> increase_level(by = 2),
          obj_to_ged(self@source_text, "TEXT") |> increase_level(by = 2),
          sprintf("1 EVEN %s", self@fact_type),
          sprintf("2 PHRASE %s", self@fact_phrase),
          sprintf("2 ROLE %s", self@role),
          sprintf("3 PHRASE %s", self@role_phrase),
          sprintf("1 QUAY %s", self@certainty),
          obj_to_ged(self@media_links, "OBJE") |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1)
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

extract_citations <- function(lines, location = NULL){
  
  sour_lst <- find_ged_values(lines, c(location, "SOUR"), return_list = TRUE)
  if(length(sour_lst) == 0) return(list())
  
  lapply(sour_lst, \(x){
    # nts <- extract_notes(x, c("SOUR","NOTE"))
    # nt_xrefs <- find_ged_values(x, c("SOUR","SNOTE"))
    # rec_date <- extract_date_value(x)
    # 
    # if(length(rec_date) == 1 && !grepl(reg_custom_value(), rec_date)){
    #   rec_date <- toupper(rec_date)
    #   rec_date <- sub("@#DGREGORIAN@ ", "", rec_date)
    # }
    # role <- find_ged_values(x, c("SOUR","EVEN","ROLE"))
    # if(length(role) == 1 && !grepl(reg_custom_value(), role)){
    #   role <- toupper(role)
    # }
    
    class_citation(
      sour_xref = find_ged_values(x, "SOUR"),
      where = find_ged_values(x, c("SOUR","PAGE")),
      date = extract_date_value(x, c("SOUR","DATA")),
      source_text = extract_translations(x, c("SOUR","DATA")),
      fact_type = find_ged_values(x, c("SOUR","EVEN")),
      fact_phrase = find_ged_values(x, c("SOUR","EVEN","PHRASE")),
      role = find_ged_values(x, c("SOUR","EVEN","ROLE")),
      role_phrase = find_ged_values(x, c("SOUR","EVEN","ROLE","PHRASE")),
      certainty = find_ged_values(x, c("SOUR","QUAY")),
      media_links = extract_media_links(x, "SOUR"),
      note_xrefs = find_ged_values(x, c("SOUR","SNOTE")),
      notes = extract_notes(x, "SOUR")
    )
  })
  
}


#' Create a shared note record object
#' 
#' @inheritParams prop_definitions
#' @param notes Not used.
#' @param note_xrefs Not used.
#' @param media_links Not used.
#' @return An S7 object representing a GEDCOM SHARED_NOTE_RECORD.
#' @export
#' @include cls_record.R cls_translation.R
#' @tests
#' expect_snapshot_value(class_record_note("@N4@",
#'                                         text = "The note goes something like this",
#'                                         language = "en")@as_ged, "json2")
class_record_note <- S7::new_class(
  "class_record_note", 
  package = "gedcomS7",
  parent = class_record,
  properties = list(
    text = S7::new_property(S7::class_character,
                            validator = function(value){
                              chk_input_size(value, 1, 1, 1)
                            }),
    media_type = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    c(
                                      chk_input_size(value, 0, 1),
                                      chk_input_choice(value, c("text/plain","text/html"))
                                    )
                                  }),
    language = S7::new_property(S7::class_character,
                                validator = function(value){
                                  c(
                                    chk_input_size(value, 0, 1, 1)
                                    #TODO: language option
                                  )
                                }),
    translations = S7::new_property(S7::class_list | class_translation_txt,
                                    validator = function(value){
                                      chk_input_S7classes(value, class_translation_txt)
                                    }),
    
    as_ged = S7::new_property(
      S7::class_data.frame,
      getter = function(self){
        c(
          sprintf("0 %s SNOTE %s", self@xref, self@text),
          sprintf("1 RESN %s", self@restrictions), # coming soon
          sprintf("1 MIME %s", self@media_type),
          sprintf("1 LANG %s", self@language),
          obj_to_ged(self@translations) |> increase_level(by = 1),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1),
          self@ids |> increase_level(by = 1),
          obj_to_ged(self@updated) |> increase_level(by = 1),
          obj_to_ged(self@created) |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    if(length(self@notes) > 0)
      return("This record does not use @notes")
    
    if(length(self@note_xrefs) > 0)
      return("This record does not use @note_xrefs")
    
    if(length(self@media_links) > 0)
      return("This record does not use @media_links")
  }
)

extract_record_note <- function(rec_lines){
  
  rec <- class_record_note(
    xref = extract_ged_xref(rec_lines[1]),
    text = extract_ged_value(rec_lines[1]),
    media_type = find_ged_values(rec_lines, "MIME"),
    language = find_ged_values(rec_lines, "LANG"),
    translations = extract_translations(rec_lines)
  )
  
  extract_common_record_elements(rec, rec_lines)
}
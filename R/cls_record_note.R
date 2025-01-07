
#' Create a shared note record object
#' 
#' @inheritParams prop_definitions
#' @param notes Not used.
#' @param note_xrefs Not used.
#' @param media_links Not used.
#' @returns An S7 object representing a GEDCOM SHARED_NOTE_RECORD.
#' @export
#' @tests
#' expect_snapshot_value(NoteRecord("@N4@",
#'                                         text = "The note goes something like this",
#'                                         language = "en")@GEDCOM, "json2")
#' expect_error(NoteRecord("test", translations = TranslationText("Woohoo")),
#'              regexp = "Each @translation requires a @language or @media_type")
NoteRecord <- S7::new_class(
  "NoteRecord", 
  parent = Record,
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
                                  )
                                }),
    translations = S7::new_property(S7::class_list,
                                    getter = function(self) self@translations,
                                    setter = function(self, value){
                                      self@translations <- as.S7class_list(value, gedcomS7::TranslationText)
                                      self
                                    },
                                    validator = function(value){
                                      for(inp in value) if(is.character(inp)) return(inp)
                                      
                                      for(tran in value){
                                        if(length(tran@language) + length(tran@media_type) == 0)
                                          return("Each @translation requires a @language or @media_type to be defined.")
                                      }
                                    }),
    
    GEDCOM = S7::new_property(
      S7::class_data.frame,
      getter = function(self){
        c(
          sprintf("0 %s SNOTE %s", self@xref, self@text),
          sprintf("1 RESN %s", self@RESTRICTIONS), # coming soon
          sprintf("1 MIME %s", self@media_type),
          sprintf("1 LANG %s", self@language),
          obj_to_ged(self@translations) |> increase_level(by = 1),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1),
          self@GEDCOM_IDENTIFIERS |> increase_level(by = 1),
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

parse_record_note <- function(rec_lines){
  
  rec <- NoteRecord(
    xref = parse_line_xref(rec_lines[1]),
    text = parse_line_value(rec_lines[1]),
    media_type = find_ged_values(rec_lines, "MIME"),
    language = find_ged_values(rec_lines, "LANG"),
    translations = parse_translations(rec_lines)
  )
  
  parse_common_record_elements(rec, rec_lines)
}

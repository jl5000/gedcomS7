
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
#' expect_error(NoteRecord("@N4@", text = "test",
#'                        translations = TranslationText("Woohoo")),
#'              regexp = "Each @translation requires a @language or @media_type")
NoteRecord <- S7::new_class(
  "NoteRecord", 
  parent = Record,
  properties = list(
    text = prop_char(1, 1, 1),
    media_type = prop_char(0, 1, choices = c("text/plain","text/html")),
    language = prop_char(0, 1, 1),
    translations = prop_S7list("translations", TranslationText),
    
    GEDCOM = S7::new_property(
      S7::class_data.frame,
      getter = function(self){
        c(
          sprintf("0 %s SNOTE %s", self@XREF, self@text),
          sprintf("1 RESN %s", restrictions_to_resn(self@confidential, self@locked, self@private)), # coming soon
          sprintf("1 MIME %s", self@media_type),
          sprintf("1 LANG %s", self@language),
          obj_to_ged(self@translations) |> level_up(1),
          obj_to_ged(self@citations) |> level_up(1),
          identifiers_to_ged(self@user_ids, self@unique_ids, self@ext_ids) |> level_up(1),
          obj_to_ged(self@updated) |> level_up(1),
          obj_to_ged(self@created) |> level_up(1)
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
    
    for(tran in self@translations){
      if(length(tran@language) + length(tran@media_type) == 0)
        return("Each @translation requires a @language or @media_type to be defined.")
    }
  }
)

parse_record_note <- function(rec_lines){
  
  rec <- NoteRecord(
    XREF = parse_line_xref(rec_lines[1]),
    text = parse_line_value(rec_lines[1]),
    media_type = find_ged_values(rec_lines, "MIME"),
    language = find_ged_values(rec_lines, "LANG"),
    translations = parse_translations(rec_lines)
  )
  
  parse_common_record_elements(rec, rec_lines)
}

S7::method(summary, NoteRecord) <- function(object, ...){
  exdent <- 15
  to_console("XREF:", object@XREF, exdent)
  to_console("Note:", object@text, exdent)
  cat("\n")
  to_console("Language:", object@language, exdent)
  to_console("Format:", object@media_type, exdent)
  to_console("Translations:", length(object@translations), exdent)
  to_console("Citations:", length(object@citations), exdent)
  cat("\n")
  print_record_summary(object)
}

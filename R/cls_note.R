
#' Create a note structure object
#' 
#' @details The shared note (SNOTE) alternative of this structure is defined
#' separately in relevant structures.
#' 
#' @inheritParams prop_definitions 
#' @returns An S7 object representing a GEDCOM NOTE_STRUCTURE.
#' @export
#' @tests
#' expect_error(Note(), regexp = "@text has too few elements")
#' expect_error(Note(letters[1:2]), regexp = "@text has too many elements")
#' expect_error(Note("test", translations = TranslationText("Ole")),
#'              regexp = "Each @translation requires a @language or @media_type")
#' expect_snapshot_value(Note("test")@GEDCOM, "json2")
#' expect_snapshot_value(Note("test", language = "en")@GEDCOM, "json2")
#' expect_snapshot_value(Note("test", 
#'                                  language = "en",
#'                                  translations = TranslationText("test",
#'                                                                   language = "en"))@GEDCOM, "json2")
#' expect_snapshot_value(Note("test", 
#'                                  language = "en",
#'                                  translations = list(TranslationText("test",
#'                                                                   language = "en"),
#'                                                  TranslationText("test2",
#'                                                                   language = "en")))@GEDCOM, "json2")
#' expect_snapshot_value(Note("test", 
#'                                  citations = SourceCitation("@S1@", 
#'                                                             notes = Note("note text 2", 
#'                                                                                citations = SourceCitation("@S4@"))))@GEDCOM, 
#'                      "json2")
#' expect_error(Note("test", 
#'                         language = "en",
#'                         translations = Address("street")),
#'              regexp = "@translations contains an invalid object")
#' expect_error(Note("test", 
#'                         language = "en",
#'                         translations = list(TranslationText("test",
#'                                                               language = "en"),
#'                                         Address("street"))),
#'              regexp = "@translations contains an invalid object not of class TranslationText")
Note <- S7::new_class(
  "Note",
  parent = GedcomS7class,
  properties = list(
    text = prop_char(1, 1, 1),
    language = prop_char(0, 1, 1),
    media_type = prop_char(0, 1, choices = c("text/plain","text/html")),
    translations = prop_S7list("translations", TranslationText),
    citations = prop_S7list("citations", SourceCitation),

    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 NOTE %s", self@text),
          sprintf("1 MIME %s", self@media_type),
          sprintf("1 LANG %s", self@language),
          obj_to_ged(self@translations) |> level_up(1),
          obj_to_ged(self@citations) |> level_up(1)
        )
      })
  ),
  validator = function(self){
    for(tran in self@translations){
      if(length(tran@language) + length(tran@media_type) == 0)
        return("Each @translation requires a @language or @media_type to be defined.")
    }
  }
)

# Need location when top level has no xref
parse_notes <- function(lines, location = NULL){
  note_lst <- find_ged_values(lines, c(location, "NOTE"), return_list = TRUE)

  lapply(note_lst, \(x){
    Note(
      text = find_ged_values(x, "NOTE"),
      language = find_ged_values(x, c("NOTE","LANG")),
      media_type = find_ged_values(x, c("NOTE","MIME")),
      translations = parse_translations(x, "NOTE"),
      citations = parse_citations(x, "NOTE")
    )
  })
  
}


S7::method(summary, Note) <- function(object, ...){
  exdent <- 16
  to_console("Note:", object@text, exdent)
  cat("\n")
  to_console("Language:", object@language, exdent)
  to_console("Format:", object@media_type, exdent)
  to_console("Translations:", length(object@translations), exdent)
  to_console("Citations:", length(object@citations), exdent)
}

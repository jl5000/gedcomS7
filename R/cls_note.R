
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
  properties = list(
    text = S7::new_property(S7::class_character,
                            validator = function(value){
                              chk_input_size(value, 1, 1, 1)
                            }),
    language = S7::new_property(S7::class_character,
                                validator = function(value){
                                  c(
                                    chk_input_size(value, 0, 1, 1)
                                  )
                                }),
    media_type = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    c(
                                      chk_input_size(value, 0, 1),
                                      chk_input_choice(value, c("text/plain","text/html"))
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
    # Using S3 because of recursion
    citations = S7::new_property(S7::class_list,
                                 getter = function(self) self@citations,
                                 setter = function(self, value){
                                   self@citations <- as.S7class_list(value, gedcomS7::SourceCitation)
                                   self
                                 },
                                 validator = function(value){
                                   for(inp in value) if(is.character(inp)) return(inp)
                                 }),

    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 NOTE %s", self@text),
          sprintf("1 MIME %s", self@media_type),
          sprintf("1 LANG %s", self@language),
          obj_to_ged(self@translations) |> increase_level(by = 1),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1)
        )
      })
  )
)

# Need location when top level has no xref
parse_notes <- function(lines, location = NULL){
  note_lst <- find_ged_values(lines, c(location, "NOTE"), return_list = TRUE)
  if(length(note_lst) == 0) return(list())
  
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

#' @include cls_validators.R
NULL

#' Create a note structure object
#' 
#' @details The shared note (SNOTE) alternative of this structure is defined
#' separately in relevant structures.
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM NOTE_STRUCTURE.
#' @include cls_translation.R cls_citation.R
#' @export
#' @tests
#' expect_error(class_note(), regexp = "@text has too few elements")
#' expect_error(class_note(letters[1:2]), regexp = "@text has too many elements")
#' expect_snapshot_value(class_note("test")@as_ged, "json2")
#' expect_snapshot_value(class_note("test", language = "en")@as_ged, "json2")
#' expect_snapshot_value(class_note("test", 
#'                                  language = "en",
#'                                  translations = class_translation_txt("test",
#'                                                                   language = "en"))@as_ged, "json2")
#' expect_snapshot_value(class_note("test", 
#'                                  language = "en",
#'                                  translations = list(class_translation_txt("test",
#'                                                                   language = "en"),
#'                                                  class_translation_txt("test2",
#'                                                                   language = "en")))@as_ged, "json2")
#' expect_snapshot_value(class_note("test", 
#'                                  citations = class_citation("@S1@", 
#'                                                             notes = class_note("note text 2", 
#'                                                                                citations = class_citation("@S4@"))))@as_ged, 
#'                      "json2")
#' expect_error(class_note("test", 
#'                         language = "en",
#'                         translations = class_address("street"))@as_ged,
#'              regexp = "@translations must be <list> or <gedcomS7::class_translation_txt>")
#' expect_error(class_note("test", 
#'                         language = "en",
#'                         translations = list(class_translation_txt("test",
#'                                                               language = "en"),
#'                                         class_address("street"))),
#'              regexp = "@translations contains an invalid object not of class_translation_txt")
class_note <- S7::new_class(
  "class_note",
  package = "gedcomS7",
  properties = list(
    text = S7::new_property(S7::class_character,
                            validator = function(value){
                              chk_input_size(value, 1, 1, 1)
                            }),
    language = S7::new_property(S7::class_character,
                                validator = function(value){
                                  c(
                                    chk_input_size(value, 0, 1)
                                    #TODO: language option
                                  )
                                }),
    media_type = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    c(
                                      chk_input_size(value, 0, 1)
                                      #TODO: media type pattern
                                    )
                                  }),
    translations = S7::new_property(S7::class_list | class_translation_txt,
                                    validator = function(value){
                                      chk_input_S7classes(value, class_translation_txt)
                                    }),
    # Using S3 because of recursion
    citations = S7::new_property(S7::class_list | 
                                   S7::new_S3_class("gedcomS7::class_citation") | 
                                   S7::class_character,
                                 validator = function(value){
                                   chk_input_S7classes(value, class_citation, reg_xref(TRUE))
                                 }),

    
    as_ged = S7::new_property(
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
extract_notes <- function(lines, location = NULL){
  note_lst <- find_ged_values(lines, c(location, "NOTE"), return_list = TRUE)
  if(length(note_lst) == 0) return(list())
  
  lapply(note_lst, \(x){
    class_note(
      text = find_ged_values(x, "NOTE"),
      language = find_ged_values(x, c("NOTE","LANG")),
      media_type = find_ged_values(x, c("NOTE","MIME")),
      translations = extract_translations(x, "NOTE"),
      citations = extract_citations(x, "NOTE")
    )
  })
  
}

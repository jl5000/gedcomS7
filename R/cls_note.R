#' @include cls_validators.R
NULL

#' Create a note structure object
#' 
#' @details The shared note (SNOTE) alternative of this structure is defined
#' separately in relevant structures.
#' 
#' In addition, this class does not include source citations as it
#' results in infinite nesting.
#' https://github.com/RConsortium/OOP-WG/issues/250
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM NOTE_STRUCTURE.
#' @include cls_translation.R
#' @export
#' @tests
#' expect_error(class_note(), regexp = "@text has too few elements")
#' expect_error(class_note(letters[1:2]), regexp = "@text has too many elements")
#' expect_snapshot_value(class_note("test")@as_ged, "json2")
#' expect_snapshot_value(class_note("test", language = "en")@as_ged, "json2")
#' expect_snapshot_value(class_note("test", 
#'                                  language = "en",
#'                                  alt_text = class_translation_txt("test",
#'                                                                   language = "en"))@as_ged, "json2")
#' expect_snapshot_value(class_note("test", 
#'                                  language = "en",
#'                                  alt_text = list(class_translation_txt("test",
#'                                                                   language = "en"),
#'                                                  class_translation_txt("test2",
#'                                                                   language = "en")))@as_ged, "json2")
#' expect_error(class_note("test", 
#'                         language = "en",
#'                         alt_text = class_address("street"))@as_ged,
#'              regexp = "@alt_text must be <list> or <gedcomS7::class_translation_txt>")
#' expect_error(class_note("test", 
#'                         language = "en",
#'                         alt_text = list(class_translation_txt("test",
#'                                                               language = "en"),
#'                                         class_address("street"))),
#'              regexp = "@alt_text contains an invalid object not of class_translation_txt")
class_note <- S7::new_class(
  "class_note",
  package = "gedcomS7",
  properties = list(
    text = S7::class_character,
    language = S7::class_character,
    media_type = S7::class_character,
    alt_text = S7::class_list | class_translation_txt,
    #citations = S7::class_list | class_citation | S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 NOTE %s", self@text),
          sprintf("1 MIME %s", self@media_type),
          sprintf("1 LANG %s", self@language),
          obj_to_ged(self@alt_text) |> increase_level(by = 1)
          #   obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@text, "@text", 1, 1, 1),
      chk_input_size(self@language, "@language", 0, 1),
      #TODO: language option
      chk_input_size(self@media_type, "@media_type", 0, 1),
      #TODO: media type pattern
      chk_input_S7classes(self@alt_text, "@alt_text", class_translation_txt)
      #  chk_input_S7classes(self@citations, "@citations", class_citation)
    )
  }
)
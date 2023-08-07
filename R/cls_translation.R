#' @include cls_validators.R
NULL

# Used in SNOTE record, SOUR record, NOTE structure, and SOURCE_CITATION
#' Create a text translation object
#' 
#' @param text A character string. New lines are created with \n.
#' @param language Optional. Language tags as defined in BCP 47.
#' @param media_type Optional. The media type as defined in RFC 2045.
#' 
#' @return An S7 object representing a GEDCOM text translation substructure.
#' @export
#' @tests
#' expect_error(class_translation_txt(), regexp = "@text has too few elements")
#' expect_error(class_translation_txt(letters[1:2]), regexp = "@text has too many elements")
#' expect_error(class_translation_txt("test"), regexp = "A text language or media_type must be defined")
#' expect_snapshot_value(class_translation_txt("test", language = "en")@as_ged, "json2")
class_translation_txt <- S7::new_class(
  "class_translation_txt",
  package = "gedcomS7",
  properties = list(
    text = S7::class_character,
    language = S7::class_character,
    media_type = S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 TRAN %s", self@text),
          sprintf("1 MIME %s", self@media_type),
          sprintf("1 LANG %s", self@language)
        )
      })
  ),
  validator = function(self){
    input_err <- NULL
    if(length(self@language) + length(self@media_type) == 0)
      input_err <- "A text language or media_type must be defined."
    c(
      chk_input_size(self@text, "@text", 1, 1),
      chk_input_size(self@language, "@language", 0, 1, 1),
      #TODO: language option
      chk_input_size(self@media_type, "@media_type", 0, 1, 1),
      #TODO: media type pattern
      input_err
    )
  })

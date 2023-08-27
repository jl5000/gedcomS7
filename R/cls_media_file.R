#' @include cls_validators.R
NULL

#' Create a media file object
#' 
#' @inheritParams prop_definitions
#' @return An S7 object representing a GEDCOM multimedia file substructure.
#' @export
#' @tests
#' expect_error(class_media_file(location = "media/original.mp3",
#'                               media_type = "audio/mp3",
#'                               medium = "CD"),
#'              regexp = "@medium has an invalid value")
#' expect_error(class_media_file(location = "media/original.mp3",
#'                               media_type = "audio/mp3",
#'                               medium_phrase = "My CD of things"),
#'              regexp = "@medium_phrase requires a @medium")
#' expect_snapshot_value(class_media_file(location = "media/original.mp3",
#'                                        title = "My audio",
#'                                        media_type = "audio/mp3",
#'                                        medium = "ELECTRONIC",
#'                                        medium_phrase = "My CD of things",
#'                                        media_alt = c("audio/ogg" = "media/derived.oga",
#'                                                      "text/vtt" = "media/transcript.vtt"))@as_ged,
#'                        "json2")
class_media_file <- S7::new_class(
  "class_media_file",
  package = "gedcomS7",
  properties = list(
    location = S7::class_character,
    title = S7::class_character,
    media_type = S7::class_character,
    medium = S7::class_character,
    medium_phrase = S7::class_character,
    media_alt = S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 FILE %s", self@location),
          sprintf("1 FORM %s", self@media_type),
          sprintf("2 MEDI %s", self@medium),
          sprintf("3 PHRASE %s", self@medium_phrase),
          sprintf("1 TITL %s", self@title),
          named_vec_to_ged(self@media_alt, "TRAN", "FORM") |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@location, "@location", 1, 1, 1),
      chk_input_size(self@media_type, "@media_type", 1, 1),
      #chk_input_choice(self@media_type, "@media_type", val_multimedia_formats()), TODO
      chk_input_size(self@medium, "@medium", 0, 1),
      chk_input_choice(self@medium, "@medium", val_medium_types()),
      chk_input_size(self@medium_phrase, "@medium_phrase", 0, 1, 1),
      chk_input_parents(self@medium_phrase, "@medium_phrase", self@medium, "@medium"),
      chk_input_size(self@title, "@title", 0, 1, 1),
      chk_input_size(self@media_alt, "@media_alt", min_val = 1)
      #chk_input_choice(names(self@media_alt), "@media_alt types", val_multimedia_formats())
    )
  }
)

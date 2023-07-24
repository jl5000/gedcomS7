#' @include cls_validators.R
NULL

#' @export
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
      input_err <- "A note language or media_type must be defined."
    c(
      chk_input_size(self@text, "@text", 1, 1),
      chk_input_size(self@language, "@language", 0, 1, 1),
      #TODO: language option
      chk_input_size(self@media_type, "@media_type", 0, 1, 1),
      #TODO: media type pattern
      input_err
    )
  })

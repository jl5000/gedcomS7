#' @include cls_validators.R
NULL

#' @export
#' @include cls_translation.R
class_note <- S7::new_class(
  "class_note",
  package = "gedcomS7",
  properties = list(
    text = S7::class_character,
    language = S7::class_character,
    media_type = S7::class_character,
    alt_text = S7::class_list,
    #citations = S7::class_list,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 NOTE %s", self@text),
          sprintf("1 MIME %s", self@media_type),
          sprintf("1 LANG %s", self@language),
          lst_to_ged(self@alt_text) |> increase_level(by = 1)
          #   lst_to_ged(self@citations) |> increase_level(by = 1)
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
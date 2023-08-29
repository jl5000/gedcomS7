

#' @export
#' @include cls_record.R cls_translation.R cls_citation.R
class_record_note <- S7::new_class(
  "class_record_note", 
  package = "gedcomS7",
  parent = class_record,
  properties = list(
    text = S7::class_character,
    media_type = S7::class_character,
    language = S7::class_character,
    text_alt = S7::class_list | class_translation_txt,
    citations = S7::class_list | class_citation | S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_data.frame,
      getter = function(self){
        c(
          sprintf("0 %s SNOTE %s", self@prim_uid, self@text),
          sprintf("1 RESN %s", self@restrictions),
          sprintf("1 MIME %s", self@media_type),
          sprintf("1 LANG %s", self@language),
          obj_to_ged(self@text_alt) |> increase_level(by = 1),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1),
          self@ids |> increase_level(by = 1),
          obj_to_ged(self@updated) |> increase_level(by = 1),
          obj_to_ged(self@created) |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@text, "@text", 1, 1, 1),
      chk_input_size(self@language, "@language", 0, 1, 1),
      #TODO: language option
      chk_input_size(self@media_type, "@media_type", 0, 1, 1),
      #TODO: media type pattern (text/plain or text/html)
      chk_input_S7classes(self@text_alt, "@text_alt", class_translation_txt),
      chk_input_S7classes(self@citations, "@citations", class_citation, reg_uuid(TRUE))
    )
  }
)


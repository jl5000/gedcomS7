#' @include cls_validators.R
NULL

#' @export
class_media_file <- S7::new_class(
  "class_media_file",
  package = "gedcomS7",
  properties = list(
    file_ref = S7::class_character,
    format = S7::class_character,
    medium = S7::class_character,
    title = S7::class_character,
    titles_alt = S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 FILE %s", self@file_ref),
          sprintf("1 FORM %s", self@format),
          named_vec_to_ged(self@medium, "MEDI", "PHRASE") |> increase_level(by = 2),
          sprintf("1 TITL %s", self@title),
          named_vec_to_ged(self@titles_alt, "TRAN", "FORM") |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@file_ref, "@file_ref", 1, 1, 1),
      chk_input_size(self@format, "@format", 1, 1, 1),
      #chk_input_choice(self@format, "@format", val_multimedia_formats()), TODO
      chk_input_size(self@medium, "@medium", 0, 1),
      chk_input_choice(self@medium, "@medium", val_medium_types()),
      chk_input_size(self@title, "@title", 0, 1, 1),
      chk_input_size(self@titles_alt, "@titles_alt", 0, 1, 1)
      #chk_input_choice(names(self@titles_alt), "@titles_alt types", val_source_media_types()),
    )
  }
)
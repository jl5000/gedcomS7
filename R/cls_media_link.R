#' @include cls_validators.R
NULL

#' @export
class_media_link <- S7::new_class(
  "class_media_link",
  package = "gedcomS7",
  properties = list(
    media_uid = S7::class_character,
    title = S7::class_character,
    crop = S7::new_property(S7::class_logical, default = FALSE),
    top = S7::new_property(S7::class_numeric, default = 0),
    left = S7::new_property(S7::class_numeric, default = 0),
    height = S7::class_numeric,
    width = S7::class_numeric,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 OBJE %s", self@media_uid),
          rep("1 CROP", self@crop),
          rep(sprintf("2 TOP %s", self@top), self@crop),
          rep(sprintf("2 LEFT %s", self@left), self@crop),
          rep(sprintf("2 HEIGHT %s", self@height), self@crop),
          rep(sprintf("2 WIDTH %s", self@width), self@crop),
          sprintf("1 TITL %s", self@title)
        )
      })
  ),
  validator = function(self) {
    c(
      chk_input_size(self@media_uid, "@media_uid", 1, 1),
      chk_input_size(self@title, "@title", 0, 1, 1),
      chk_input_size(self@crop, "@crop", 1, 1),
      chk_input_size(self@top, "@top", 0, 1, 0),
      chk_input_size(self@left, "@left", 0, 1, 0),
      chk_input_size(self@height, "@height", 0, 1, 1),
      chk_input_size(self@width, "@width", 0, 1, 1),
      chk_whole_number(self@top, "@top"),
      chk_whole_number(self@left, "@left"),
      chk_whole_number(self@height, "@height"),
      chk_whole_number(self@width, "@width"),
      chk_input_pattern(self@media_uid, "@media_uid", reg_uuid(TRUE))
    )
  }
)

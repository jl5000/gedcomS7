#' @include cls_validators.R
NULL

#' Create a multimedia link object
#' 
#' @details
#' The properties @left and @top indicate the top left corner of the region to display.
#' The properties @width and @height indicate the dimensions of the region to display.
#' 
#' If the multimedia record contains multiple files, then the crop parameters only 
#' applies to the first file.
#'  
#' @param media_uid The UID of a multimedia record.
#' @param title The title of the multimedia record. This will supercede any title given
#' in the record.
#' @param crop Whether to crop the multimedia to a specific area.
#' @param top The number of pixels to omit from the top side of the image.
#' @param left The number of pixels to omit from the left side of the image.
#' @param height The height in pixels of the cropped region.
#' @param width The width in pixels of the cropped region.
#' 
#' @return An S7 object representing a GEDCOM MULTIMEDIA_LINK.
#' @export
#' @tests
#' expect_error(class_media_link(), regexp = "@media_uid has too few elements")
#' expect_error(class_media_link("@O4@"), regexp = "@media_uid is in an invalid format")
#' expect_snapshot_value(class_media_link(uuid::UUIDgenerate())@as_ged, "json2")
#' expect_snapshot_value(class_media_link(uuid::UUIDgenerate(), title = "new title")@as_ged, "json2")
#' expect_snapshot_value(class_media_link(uuid::UUIDgenerate(), 
#'                                        title = "new title",
#'                                        crop = TRUE)@as_ged, "json2")
#' expect_snapshot_value(class_media_link(uuid::UUIDgenerate(), 
#'                                        title = "new title",
#'                                        crop = TRUE,
#'                                        top = 5, left = 200)@as_ged, "json2")
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

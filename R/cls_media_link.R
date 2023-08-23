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
#' @inheritParams prop_definitions
#' @return An S7 object representing a GEDCOM MULTIMEDIA_LINK.
#' @export
#' @tests
#' expect_snapshot_value(class_media_link()@as_ged, "json2")
#' expect_error(class_media_link("@O4"), regexp = "@media_xref is in an invalid format")
#' expect_snapshot_value(class_media_link("@1@")@as_ged, "json2")
#' expect_snapshot_value(class_media_link("@1@", 
#'                                        title = "new title")@as_ged, "json2")
#' expect_snapshot_value(class_media_link("@1@", 
#'                                        title = "new title",
#'                                        crop = TRUE)@as_ged, "json2")
#' expect_snapshot_value(class_media_link("@1@", 
#'                                        title = "new title",
#'                                        crop = TRUE,
#'                                        top = 5, left = 200)@as_ged, "json2")
class_media_link <- S7::new_class(
  "class_media_link",
  package = "gedcomS7",
  properties = list(
    media_xref = S7::new_property(S7::class_character, default = "@VOID@"),
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
          sprintf("0 OBJE %s", self@media_xref),
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
      chk_input_size(self@media_xref, "@media_xref", 1, 1),
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
      chk_input_pattern(self@media_xref, "@media_xref", reg_xref(TRUE))
    )
  }
)

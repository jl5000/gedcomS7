
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
#' @param media_xref The cross-reference identifier of a multimedia record.
#' @param top The number of pixels to omit from the top side of the image.
#' @param left The number of pixels to omit from the left side of the image.
#' @param height The height in pixels of the cropped region.
#' @param width The width in pixels of the cropped region.
#' 
#' @returns An S7 object representing a GEDCOM MULTIMEDIA_LINK.
#' @export
#' @tests
#' expect_snapshot_value(MediaLink()@GEDCOM, "json2")
#' expect_error(MediaLink("@O4"), regexp = "@media_xref is in an invalid format")
#' expect_snapshot_value(MediaLink("@1@")@GEDCOM, "json2")
#' expect_snapshot_value(MediaLink("@1@", 
#'                                        title = "new title")@GEDCOM, "json2")
#' expect_snapshot_value(MediaLink("@1@", 
#'                                        title = "new title",
#'                                        top = 5, left = 200)@GEDCOM, "json2")
MediaLink <- S7::new_class(
  "MediaLink",
  parent = GedcomS7class,
  properties = list(
    media_xref = S7::new_property(S7::class_character, default = "@VOID@",
                                  validator = function(value){
                                    c(
                                      chk_input_size(value, 1, 1),
                                      chk_input_pattern(value, reg_xref(TRUE))
                                    )
                                  }),
    title = S7::new_property(S7::class_character,
                             validator = function(value){
                               chk_input_size(value, 0, 1, 1)
                             }),
    top = S7::new_property(S7::class_numeric,
                           validator = function(value){
                             c(
                               chk_input_size(value, 0, 1, 0),
                               chk_whole_number(value)
                             )
                           }),
    left = S7::new_property(S7::class_numeric,
                            validator = function(value){
                              c(
                                chk_input_size(value, 0, 1, 0),
                                chk_whole_number(value)
                              )
                            }),
    height = S7::new_property(S7::class_numeric,
                              validator = function(value){
                                c(
                                  chk_input_size(value, 0, 1, 1),
                                  chk_whole_number(value)
                                )
                              }),
    width = S7::new_property(S7::class_numeric,
                             validator = function(value){
                               c(
                                 chk_input_size(value, 0, 1, 1),
                                 chk_whole_number(value)
                               )
                             }),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 OBJE %s", self@media_xref),
          rep("1 CROP", length(self@top) + length(self@left) +
                        length(self@height) + length(self@width) > 0),
          sprintf("2 TOP %s", self@top),
          sprintf("2 LEFT %s", self@left),
          sprintf("2 HEIGHT %s", self@height),
          sprintf("2 WIDTH %s", self@width),
          sprintf("1 TITL %s", self@title)
        )
      })
  )
)

parse_media_links <- function(lines, location = NULL){
  media_lst <- find_ged_values(lines, c(location, "OBJE"), return_list = TRUE)
  if(length(media_lst) == 0) return(list())
  
  lapply(media_lst, \(x){
    MediaLink(
      media_xref = find_ged_values(x, "OBJE"),
      top = find_ged_values(x, c("OBJE","CROP","TOP")) |> as.numeric(),
      left = find_ged_values(x, c("OBJE","CROP","LEFT")) |> as.numeric(),
      height = find_ged_values(x, c("OBJE","CROP","HEIGHT")) |> as.numeric(),
      width = find_ged_values(x, c("OBJE","CROP","WIDTH")) |> as.numeric(),
      title = find_ged_values(x, c("OBJE","TITL"))
    )
  })
  
}

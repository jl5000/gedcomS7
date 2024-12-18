

#' Create a media file object
#' 
#' @inheritParams prop_definitions
#' @return An S7 object representing a GEDCOM multimedia file substructure.
#' @export
#' @tests
#' expect_error(MediaFile(location = "media/original.mp3",
#'                               media_type = "audio/mp3",
#'                               medium = "CD"),
#'              regexp = "@medium has an invalid value")
#' expect_error(MediaFile(location = "media/original.mp3",
#'                               media_type = "audio/mp3",
#'                               medium_phrase = "My CD of things"),
#'              regexp = "@medium_phrase requires a @medium")
#' expect_snapshot_value(MediaFile(location = "media/original.mp3",
#'                                        title = "My audio",
#'                                        media_type = "audio/mp3",
#'                                        medium = "ELECTRONIC",
#'                                        medium_phrase = "My CD of things",
#'                                        media_alt = c("audio/ogg" = "media/derived.oga",
#'                                                      "text/vtt" = "media/transcript.vtt"))@c_as_ged,
#'                        "json2")
MediaFile <- S7::new_class(
  "MediaFile",
  properties = list(
    location = S7::new_property(S7::class_character,
                                validator = function(value){
                                  chk_input_size(value, 1, 1, 1)
                                }),
    title = S7::new_property(S7::class_character,
                             validator = function(value){
                               chk_input_size(value, 0, 1, 1)
                             }),
    media_type = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    c(
                                      chk_input_size(value, 1, 1, 1)
                                      #chk_input_choice(value, val_multimedia_formats()), TODO
                                    )
                                  }),
    medium = S7::new_property(S7::class_character,
                              validator = function(value){
                                c(
                                  chk_input_size(value, 0, 1),
                                  chk_input_choice(value, val_medium_types())
                                )
                              }),
    medium_phrase = S7::new_property(S7::class_character,
                                     validator = function(value){
                                       chk_input_size(value, 0, 1, 1)
                                     }),
    media_alt = S7::new_property(S7::class_character,
                                 validator = function(value){
                                   c(
                                     chk_input_size(value, min_val = 1)
                                     #chk_input_choice(names(value), val_multimedia_formats())
                                   )
                                 }),
    
    c_as_ged = S7::new_property(
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
    chk_input_parents(self@medium_phrase, "@medium_phrase", self@medium, "@medium")
  }
)

#' Create a multimedia record object
#' 
#' @inheritParams prop_definitions 
#' @param media_links Not used.
#' @return An S7 object representing a GEDCOM MULTIMEDIA_RECORD.
#' @export
#' @tests
#' fls <- list(MediaFile(location = "media/original.mp3",
#'                                        title = "My audio",
#'                                        media_type = "audio/mp3",
#'                                        medium = "ELECTRONIC",
#'                                        medium_phrase = "My CD of things",
#'                                        media_alt = c("audio/ogg" = "media/derived.oga",
#'                                                      "text/vtt" = "media/transcript.vtt")),
#'             MediaFile(location = "media/speech.mp3",
#'                              media_type = "audio/mp3")
#'            )
#'            
#' expect_snapshot_value(MediaRecord("@M548@", files = fls,
#'                                          locked = TRUE,
#'                                          notes = "Very loud")@c_as_ged, "json2")            
MediaRecord <- S7::new_class(
  "MediaRecord", 
  parent = Record,
  properties = list(
    files = S7::new_property(S7::class_list | 
                               S7::new_S3_class("gedcomS7::MediaFile"),
                             validator = function(value){
                               c(
                                 chk_input_size(value, 1),
                                 chk_input_S7classes(value, MediaFile)
                               )
                             }),
    
    c_as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s OBJE", self@xref),
          sprintf("1 RESN %s", self@c_restrictions),
          obj_to_ged(self@files) |> increase_level(by = 1),
          self@c_ids_as_ged |> increase_level(by = 1),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1),
          obj_to_ged(self@updated) |> increase_level(by = 1),
          obj_to_ged(self@created) |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    if(length(self@media_links) > 0)
      return("This record does not use @media_links")
  }
)

parse_media_files <- function(rec_lines){
  file_lst <- find_ged_values(rec_lines, "FILE", return_list = TRUE)
  if(length(file_lst) == 0) return(list())
  
  lapply(file_lst, \(x){
    MediaFile(
      location = find_ged_values(x, "FILE"),
      title = find_ged_values(x, c("FILE","TITL")),
      media_type = find_ged_values(x, c("FILE","FORM")),
      medium = find_ged_values(x, c("FILE","FORM","MEDI")),
      medium_phrase = find_ged_values(x, c("FILE","FORM","MEDI","PHRASE")),
      media_alt = parse_vals_and_types(x, c("FILE","TRAN"))
    )
  })
  
}

parse_record_media <- function(rec_lines){
  
  rec <- MediaRecord(
    xref = parse_line_xref(rec_lines[1]),
    files = parse_media_files(rec_lines)
  )
  
  parse_common_record_elements(rec, rec_lines)
}

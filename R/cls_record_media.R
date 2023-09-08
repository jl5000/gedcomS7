

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

#' Create a multimedia record object
#' 
#' @inheritParams prop_definitions 
#' @param media_links Not used.
#' @return An S7 object representing a GEDCOM MULTIMEDIA_RECORD.
#' @export
#' @include cls_record.R
#' @tests
#' fls <- list(class_media_file(location = "media/original.mp3",
#'                                        title = "My audio",
#'                                        media_type = "audio/mp3",
#'                                        medium = "ELECTRONIC",
#'                                        medium_phrase = "My CD of things",
#'                                        media_alt = c("audio/ogg" = "media/derived.oga",
#'                                                      "text/vtt" = "media/transcript.vtt")),
#'             class_media_file(location = "media/speech.mp3",
#'                              media_type = "audio/mp3")
#'            )
#'            
#' expect_snapshot_value(class_record_media("@M548@", files = fls,
#'                                          locked = TRUE,
#'                                          notes = "Very loud")@as_ged, "json2")            
class_record_media <- S7::new_class(
  "class_record_media", 
  package = "gedcomS7",
  parent = class_record,
  properties = list(
    files = S7::class_list | class_media_file,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s OBJE", self@xref),
          sprintf("1 RESN %s", self@restrictions),
          obj_to_ged(self@files) |> increase_level(by = 1),
          self@ids |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1),
          obj_to_ged(self@updated) |> increase_level(by = 1),
          obj_to_ged(self@created) |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@files, "@files", 1),
      chk_input_S7classes(self@files, "@files", class_media_file),
      chk_input_size(self@media_links, "@media_links", 0, 0)
    )
  }
)

extract_record_media <- function(rec_lines){
  
  rec <- class_record_media(
    xref = extract_ged_xref(rec_lines[1]),
    files = extract_media_files(rec_lines)
  )
  
  extract_common_record_elements(rec, rec_lines)
}
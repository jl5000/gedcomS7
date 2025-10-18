

#' Create a media file object
#' 
#' @inheritParams prop_definitions
#' @param location An absolute or relative URL to the file.
#' @param medium A value from `val_medium_types()`. If "OTHER" is selected then a `@medium_phrase`
#' must be given. This should describe the original medium from which it was derived. So
#' if it is a digital image scanned from a physical photograph, it should be "PHOTO"
#' instead of "ELECTRONIC".
#' 
#' @returns An S7 object representing a GEDCOM multimedia file substructure.
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
#' expect_error(MediaFile(location = "media/original.mp3",
#'                               media_type = "audio/mp3",
#'                               medium = "OTHER"),
#'              regexp = "A @medium_phrase must be given if @medium is 'OTHER'")
#' expect_snapshot_value(MediaFile(location = "media/original.mp3",
#'                                        title = "My audio",
#'                                        media_type = "audio/mp3",
#'                                        medium = "ELECTRONIC",
#'                                        medium_phrase = "My CD of things",
#'                                        media_alt = c("audio/ogg" = "media/derived.oga",
#'                                                      "text/vtt" = "media/transcript.vtt"))@GEDCOM,
#'                        "json2")
MediaFile <- S7::new_class(
  "MediaFile",
  parent = GedcomS7class,
  properties = list(
    location = prop_char(1, 1, 1),
    title = prop_char(0, 1, 1),
    media_type = prop_char(1, 1, 1),
    medium = prop_char(0, 1, choices = val_medium_types()),
    medium_phrase = prop_char(0, 1, 1),
    media_alt = prop_char(min_char = 1),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 FILE %s", self@location),
          sprintf("1 FORM %s", self@media_type),
          sprintf("2 MEDI %s", self@medium),
          sprintf("3 PHRASE %s", self@medium_phrase),
          sprintf("1 TITL %s", self@title),
          as_ged(self@media_alt, "TRAN", "FORM") |> level_up(1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_phrase(self@medium_phrase, "@medium_phrase",
                       self@medium, "@medium", "OTHER"),
      chk_input_parents(self@medium_phrase, "@medium_phrase", self@medium, "@medium")
    )
  }
)

#' Create a multimedia record object
#' 
#' @inheritParams prop_definitions 
#' @param media_links Not used.
#' @param files A `MediaFile` object or a list of them. This refers to 1 or more external 
#' digital files. Grouped files should each pertain to the same context.
#'
#' @returns An S7 object representing a GEDCOM MULTIMEDIA_RECORD.
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
#'                                          notes = "Very loud")@GEDCOM, "json2")            
MediaRecord <- S7::new_class(
  "MediaRecord", 
  parent = Record,
  properties = list(
    files = prop_S7list("files", MediaFile),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s OBJE", self@XREF),
          sprintf("1 RESN %s", restrictions_to_resn(self@confidential, self@locked, self@private)),
          as_ged(self@files) |> level_up(1),
          identifiers_ged(self@user_ids, self@unique_ids, self@ext_ids) |> level_up(1),
          notes_ged(self@notes, self@note_xrefs) |> level_up(1),
          as_ged(self@citations) |> level_up(1),
          as_ged(self@updated) |> level_up(1),
          as_ged(self@created) |> level_up(1)
        )
      })
  ),
  validator = function(self){
    if(length(self@media_links) > 0)
      return("This record does not use @media_links")
    
    if(length(self@files) < 1)
      return("At least one file must be defined")
  }
)

parse_media_files <- function(rec_lines){
  file_lst <- find_ged_values(rec_lines, "FILE", return_list = TRUE)

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
    XREF = parse_line_xref(rec_lines[1]),
    files = parse_media_files(rec_lines)
  )
  
  parse_common_record_elements(rec, rec_lines)
}

S7::method(summary, MediaRecord) <- function(object, ...){
  exdent <- 15
  to_console("XREF:", object@XREF, exdent)
  to_console_list("Files:", object@files, exdent, prop = "location")
  cat("\n")
  to_console("Citations:", length(object@citations), exdent)
  to_console("Notes:", length(object@notes) + length(object@note_xrefs), exdent)
  cat("\n")
  print_record_summary(object)
}

S7::method(summary, MediaFile) <- function(object, ...){
  exdent <- 15
  to_console("Location:", object@location, exdent)
  to_console("Title:", object@title, exdent)
  to_console("Format:", object@media_type, exdent)
  to_console_value_with_phrase("Medium:", 
                               object@medium, object@medium_phrase, 
                               exdent)
}

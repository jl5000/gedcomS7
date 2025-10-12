
#' Create a place structure object
#' 
#' @inheritParams prop_definitions 
#' @param place_name A comma-separated string of region names, ordered from smallest to 
#' largest. The specific meaning of each element is given by the @place_form, or in the 
#' `@default_place_form` of the gedcom object if there is no @place_form defined. Elements 
#' should be left blank if they are unknown, do not apply to the location, or are too 
#' specific for the region in question. For example "Baltimore, , Maryland, USA".
#' @param place_form A comma-separated string of jurisdictional titles, which has the same 
#' number of elements as @place_form. For example "City, County, State, Country".
#' @param place_translations A named character vector of translations of the place name.
#' The vector values must follow the same form as the @place_name and the vector names
#' must be a language value as defined by @language.
#' @param lat_long The latitude and longitude of the place, separated by a space.
#' The latitude coordinate is the direction North or South from the equator in degrees and 
#' fraction of degrees. The longitude coordinate is in degrees and fraction of degrees East 
#' or West of the zero or base meridian coordinate.
#' For example: 18 degrees, 9 minutes, and 3.4 seconds North, 168 degrees, 9 minutes, and 
#' 3.4 seconds East would be formatted as "N18.150944 E168.150944".
#' 
#' @returns An S7 object representing a GEDCOM PLACE_STRUCTURE.
#' @export
#' @tests
#' expect_error(Place(), regexp = "@place_name has too few elements")
#' expect_error(Place(""), regexp = "@place_name has too few characters")
#' expect_snapshot_value(Place("here")@GEDCOM, "json2")
#' expect_error(Place("here", lat_long = "123 543"), regexp = "@lat_long is in an invalid format")
#' expect_snapshot_value(Place("here", lat_long = "N12 E56")@GEDCOM, "json2")
#' expect_error(Place("here", place_translations = "hier"), regexp = "@place_translations has too few elements")
#' expect_snapshot_value(Place("here", place_translations = c(nl = "hier", da = "her"))@GEDCOM, "json2")
#' expect_snapshot_value(Place("here", 
#'                                   place_translations = c(nl = "hier", da = "her"),
#'                                   lat_long = "N12 E56")@GEDCOM, "json2")
#' expect_snapshot_value(Place("here", 
#'                                   language = "en",
#'                                   place_translations = c(nl = "hier", da = "her"),
#'                                   lat_long = "N12 E56",
#'                                   note_xrefs = c("@N1@","@N562@"),
#'                                   notes = "Thing 1")@GEDCOM, "json2")
Place <- S7::new_class(
  "Place",
  parent = GedcomS7class,
  properties = list(
    place_name = prop_char(1, 1, 1),
    place_form = prop_char(0, 1, 1),
    language = prop_char(0, 1, 1),
    place_translations = prop_char(min_char = 1, names_required = TRUE),
    lat_long = prop_char(0, 1, pattern = sprintf("^%s %s$", reg_latitude(), reg_longitude())),
    ext_ids = prop_char(min_char = 1, names_required = TRUE),
    note_xrefs = prop_char(pattern = reg_xref(TRUE)),
    notes = prop_S7list("notes", Note),
    
    LATITUDE = S7::new_property(S7::class_character,
                           getter = function(self){
                             if(length(self@lat_long) == 0) return(character())
                             unlist(strsplit(self@lat_long, split = " "))[1]
                           }),
    LONGITUDE = S7::new_property(S7::class_character,
                            getter = function(self){
                              if(length(self@lat_long) == 0) return(character())
                              unlist(strsplit(self@lat_long, split = " "))[2]
                            }),
    
    GEDCOM_STRING = S7::new_property(S7::class_character, 
                              getter = function(self) self@place_name),
    
    GEDCOM = S7::new_property(S7::class_character,
                              getter = function(self){
                                c(
                                  sprintf("0 PLAC %s", self@place_name),
                                  sprintf("1 FORM %s", self@place_form),
                                  sprintf("1 LANG %s", self@language),
                                  named_vec_to_ged(self@place_translations, "TRAN", "LANG") |> increase_level(by = 1),
                                  rep("1 MAP", length(self@lat_long)),
                                  sprintf("2 LATI %s", self@LATITUDE),
                                  sprintf("2 LONG %s", self@LONGITUDE),
                                  named_vec_to_ged(self@ext_ids, "EXID", "TYPE") |> increase_level(by = 1),
                                  notes_to_ged(self@notes, self@note_xrefs) |> increase_level(by = 1)
                                )
                              })
  )
)

parse_place <- function(lines, location = NULL){
  
  place_name <- find_ged_values(lines, c(location, "PLAC"))
  if(length(place_name) == 0) return(NULL)
  
  latlong <- paste(
    find_ged_values(lines, c(location, "PLAC", "MAP", "LATI")),
    find_ged_values(lines, c(location, "PLAC", "MAP", "LONG"))
  )
  
  Place(
    place_name = place_name,
    place_form = find_ged_values(lines, c(location, "PLAC","FORM")),
    language = find_ged_values(lines, c(location, "PLAC","LANG")),
    place_translations = parse_vals_and_types(lines, c(location, "PLAC","TRAN")),
    lat_long = latlong,
    ext_ids = parse_vals_and_types(lines, c(location, "PLAC","EXID")),
    note_xrefs = find_ged_values(lines, c(location, "PLAC","SNOTE")),
    notes = parse_notes(lines, c(location, "PLAC"))
  )
  
}

S7::method(summary, Place) <- function(object, ...){
  exdent <- 15
  to_console("Place:", object@place_name, exdent)
  cat("\n")
  to_console("Translations:", length(object@place_translations), exdent)
  to_console("Notes:", length(object@notes) + length(object@note_xrefs), exdent)
  to_console("External IDs:", toString(paste(names(object@ext_ids), object@ext_ids, sep = "/")), exdent)
}

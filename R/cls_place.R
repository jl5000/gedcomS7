#' @include cls_validators.R
NULL

#' Create a place structure object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM PLACE_STRUCTURE.
#' @export
#' @include cls_note.R
#' @tests
#' expect_error(class_place(), regexp = "@place_name has too few elements")
#' expect_error(class_place(""), regexp = "@place_name has too few characters")
#' expect_snapshot_value(class_place("here")@as_ged, "json2")
#' expect_error(class_place("here", lat_long = "123 543"), regexp = "@lat_long is in an invalid format")
#' expect_snapshot_value(class_place("here", lat_long = "N12 E56")@as_ged, "json2")
#' expect_error(class_place("here", place_translations = "hier"), regexp = "@place_translations names has too few elements")
#' expect_snapshot_value(class_place("here", place_translations = c(nl = "hier", da = "her"))@as_ged, "json2")
#' expect_snapshot_value(class_place("here", 
#'                                   place_translations = c(nl = "hier", da = "her"),
#'                                   lat_long = "N12 E56")@as_ged, "json2")
#' expect_snapshot_value(class_place("here", 
#'                                   language = "en",
#'                                   place_translations = c(nl = "hier", da = "her"),
#'                                   lat_long = "N12 E56",
#'                                   note_xrefs = c("@N1@","@N562@"),
#'                                   notes = "Thing 1")@as_ged, "json2")
class_place <- S7::new_class(
  "class_place",
  package = "gedcomS7",
  properties = list(
    place_name = S7::class_character,
    place_form = S7::class_character,
    language = S7::class_character,
    place_translations = S7::class_character,
    lat_long = S7::class_character,
    ext_ids = S7::class_character,
    note_xrefs = S7::class_character,
    notes = S7::class_list | class_note | S7::class_character,
    
    lat = S7::new_property(S7::class_character,
                           getter = function(self){
                             if(length(self@lat_long) == 0) return(character())
                             unlist(strsplit(self@lat_long, split = " "))[1]
                           }),
    long = S7::new_property(S7::class_character,
                            getter = function(self){
                              if(length(self@lat_long) == 0) return(character())
                              unlist(strsplit(self@lat_long, split = " "))[2]
                            }),
    
    as_val = S7::new_property(S7::class_character, 
                              getter = function(self) self@place_name),
    
    as_ged = S7::new_property(S7::class_character,
                              getter = function(self){
                                c(
                                  sprintf("0 PLAC %s", self@place_name),
                                  sprintf("1 FORM %s", self@place_form),
                                  sprintf("1 LANG %s", self@language),
                                  named_vec_to_ged(self@place_translations, "TRAN", "LANG") |> increase_level(by = 1),
                                  rep("1 MAP", length(self@lat_long)),
                                  sprintf("2 LATI %s", self@lat),
                                  sprintf("2 LONG %s", self@long),
                                  named_vec_to_ged(self@ext_ids, "EXID", "TYPE") |> increase_level(by = 1),
                                  sprintf("1 SNOTE %s", self@note_xrefs),
                                  obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1)
                                )
                              })
  ),
  
  validator = function(self) {
    c(
      chk_input_size(self@place_name, "@place_name", 1, 1, 1),
      chk_input_size(self@place_form, "@place_form", 0, 1, 1),
      chk_input_size(self@language, "@language", 0, 1),
      #TODO: language lookup
      chk_input_size(self@place_translations, "@place_translations", min_val = 1),
      chk_input_size(names(self@place_translations), "@place_translations names", length(self@place_translations), length(self@place_translations)),
      #TODO: language lookup
      chk_input_size(self@lat_long, "@lat_long", 0, 1),
      chk_input_pattern(self@lat_long, "@lat_long", sprintf("^%s %s$", reg_latitude(), reg_longitude())),
      chk_input_size(self@ext_ids, "@ext_ids", min_val = 1),
      chk_input_size(names(self@ext_ids), "@ext_ids names", length(self@ext_ids), length(self@ext_ids)),
      chk_input_pattern(self@note_xrefs, "@note_xrefs", reg_xref(TRUE)),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+")
    )
  }
)

extract_place <- function(lines, location = NULL){
  
  place_name <- find_ged_values(lines, c(location, "PLAC"))
  if(length(place_name) == 0) return(character())
  
  latlong <- paste(
    find_ged_values(lines, c(location, "PLAC", "MAP", "LATI")),
    find_ged_values(lines, c(location, "PLAC", "MAP", "LONG"))
  )
  
  class_place(
    place_name = place_name,
    place_form = find_ged_values(lines, c(location, "PLAC","FORM")),
    language = find_ged_values(lines, c(location, "PLAC","LANG")),
    place_translations = extract_vals_and_types(lines, "TRAN"),
    lat_long = latlong,
    ext_ids = extract_vals_and_types(lines, "EXID"),
    note_xrefs = find_ged_values(lines, c(location, "PLAC","SNOTE")),
    notes = extract_notes(lines, c(location, "PLAC"))
  )
  
}
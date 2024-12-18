
#' Create a place structure object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM PLACE_STRUCTURE.
#' @export
#' @tests
#' expect_error(class_place(), regexp = "@place_name has too few elements")
#' expect_error(class_place(""), regexp = "@place_name has too few characters")
#' expect_snapshot_value(class_place("here")@c_as_ged, "json2")
#' expect_error(class_place("here", lat_long = "123 543"), regexp = "@lat_long is in an invalid format")
#' expect_snapshot_value(class_place("here", lat_long = "N12 E56")@c_as_ged, "json2")
#' expect_error(class_place("here", place_translations = "hier"), regexp = "@place_translations has too few elements")
#' expect_snapshot_value(class_place("here", place_translations = c(nl = "hier", da = "her"))@c_as_ged, "json2")
#' expect_snapshot_value(class_place("here", 
#'                                   place_translations = c(nl = "hier", da = "her"),
#'                                   lat_long = "N12 E56")@c_as_ged, "json2")
#' expect_snapshot_value(class_place("here", 
#'                                   language = "en",
#'                                   place_translations = c(nl = "hier", da = "her"),
#'                                   lat_long = "N12 E56",
#'                                   note_xrefs = c("@N1@","@N562@"),
#'                                   notes = "Thing 1")@c_as_ged, "json2")
class_place <- S7::new_class(
  "class_place",
  properties = list(
    place_name = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_size(value, 1, 1, 1)
                                  }),
    place_form = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_size(value, 0, 1, 1)
                                  }),
    language = S7::new_property(S7::class_character,
                                validator = function(value){
                                  c(
                                    chk_input_size(value, 0, 1, 1)
                                    #TODO: language lookup
                                  )
                                }),
    place_translations = S7::new_property(S7::class_character,
                                          validator = function(value){
                                            c(
                                              chk_input_size(value, min_val = 1),
                                              chk_input_size(names(value), length(value), length(value), 1)
                                            )
                                          }),
    lat_long = S7::new_property(S7::class_character,
                                validator = function(value){
                                  c(
                                    chk_input_size(value, 0, 1),
                                    chk_input_pattern(value, sprintf("^%s %s$", reg_latitude(), reg_longitude()))
                                  )
                                }),
    ext_ids = S7::new_property(S7::class_character,
                               validator = function(value){
                                 c(
                                   chk_input_size(value, min_val = 1),
                                   chk_input_size(names(value), length(value), length(value), 1)
                                 )
                               }),
    note_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    notes = S7::new_property(S7::class_list | 
                               S7::new_S3_class("gedcomS7::class_note") | 
                               S7::class_character,
                             validator = function(value){
                               chk_input_S7classes(value, class_note, ".+")
                             }),
    
    c_lat = S7::new_property(S7::class_character,
                           getter = function(self){
                             if(length(self@lat_long) == 0) return(character())
                             unlist(strsplit(self@lat_long, split = " "))[1]
                           }),
    c_long = S7::new_property(S7::class_character,
                            getter = function(self){
                              if(length(self@lat_long) == 0) return(character())
                              unlist(strsplit(self@lat_long, split = " "))[2]
                            }),
    
    c_as_val = S7::new_property(S7::class_character, 
                              getter = function(self) self@place_name),
    
    c_as_ged = S7::new_property(S7::class_character,
                              getter = function(self){
                                c(
                                  sprintf("0 PLAC %s", self@place_name),
                                  sprintf("1 FORM %s", self@place_form),
                                  sprintf("1 LANG %s", self@language),
                                  named_vec_to_ged(self@place_translations, "TRAN", "LANG") |> increase_level(by = 1),
                                  rep("1 MAP", length(self@lat_long)),
                                  sprintf("2 LATI %s", self@c_lat),
                                  sprintf("2 LONG %s", self@c_long),
                                  named_vec_to_ged(self@ext_ids, "EXID", "TYPE") |> increase_level(by = 1),
                                  obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
                                  sprintf("1 SNOTE %s", self@note_xrefs)
                                )
                              })
  )
)

parse_place <- function(lines, location = NULL){
  
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
    place_translations = parse_vals_and_types(lines, c(location, "PLAC","TRAN")),
    lat_long = latlong,
    ext_ids = parse_vals_and_types(lines, c(location, "PLAC","EXID")),
    note_xrefs = find_ged_values(lines, c(location, "PLAC","SNOTE")),
    notes = parse_notes(lines, c(location, "PLAC"))
  )
  
}

#' @include utils_at.R cls_validators.R
NULL

# dont need this!! named vector will do
class_place_name_trans <- S7::new_class("class_place_name_trans",
                                           properties = list(
                                             name = S7::class_character,
                                             language = S7::class_character,
                                             
                                             as_ged = S7::new_property(
                                               S7::class_character,
                                               getter = function(self){
                                                 c(
                                                   sprintf("0 TRAN %s", self@name),
                                                   sprintf("1 LANG %s", self@language)
                                                 )
                                               })
                                           ),
                                           validator = function(self){
                                             c(
                                               chk_input_size(self@name, "@name", 1, 1, 1),
                                               chk_input_size(self@language, "@language", 1, 1)
                                               #TODO: language option
                                             )
                                           })


#' Create a Place object
#' 
#' @details 
#' The latitude coordinate is
#' the direction North or South from the equator in degrees and fraction of degrees carried out
#' to give the desired accuracy. For example: 18 degrees, 9 minutes, and 3.4 seconds North
#' would be formatted as N18.150944.
#' 
#' The longitude
#' coordinate is Degrees and fraction of degrees east or west of the zero or base meridian
#' coordinate. For example: 168 degrees, 9 minutes, and 3.4 seconds East would be formatted
#' as E168.150944.
#' 
#' @param name The jurisdictional name of the place where the event took place. Jurisdictions are separated
#' by a comma and space combination. For example: "Cove, Cache, Utah, United States of
#' America".
#' No part of the place name may be replaced by an abbreviation. Place names are not
#' terminated by a full stop or anything else.
#' @param phon_names A named vector of phonetic variations of the place name written in the same form as 
#' `name`. The names of the vector contain the phonetisation method used for creating the phonetic text.
#' @param rom_names A named vector of romanised variations of the place name written in the same form as 
#' `name`. The names of the vector contain the romanisation method used for creating the romanised text.
#' @param lat_long A character string giving the latitude and longitude separated by a space. See Details.
#' @param notes A list of `class_note` objects to record or reference notes about the place.
#'
#' @return An S7 Place object.
#' @export
#' @name class_place
NULL
class_place <- S7::new_class("class_place",
                             properties = list(
                               name = S7::class_character,
                               form = S7::class_character,
                               language = S7::class_character,
                               names_alt = S7::class_character,
                               lat_long = S7::class_character,
                               external_ids = S7::class_character,
                               note_links = S7::class_character,
                               notes = S7::class_list,
                               
                               lat = S7::new_property(S7::class_character,
                                                      getter = function(self){
                                                        if(length(self@lat_long) == 1){
                                                          unlist(strsplit(self@lat_long, split = " "))[1]
                                                        } else { character() }
                                                      }),
                               long = S7::new_property(S7::class_character,
                                                       getter = function(self){
                                                         if(length(self@lat_long) == 1){
                                                           unlist(strsplit(self@lat_long, split = " "))[2]
                                                         } else { character() }
                                                       }),
                               
                               as_val = S7::new_property(S7::class_character, 
                                                         getter = function(self) self@name),
                               
                               as_ged = S7::new_property(S7::class_character,
                                                        getter = function(self){
                                                          
                                                          pla <- c(
                                                            sprintf("0 PLAC %s", self@name),
                                                            sprintf("1 FORM %s", self@form),
                                                            sprintf("1 LANG %s", self@language)
                                                          )
                                                          
                                                          for (i in seq_along(self@names_alt)) {
                                                            pla <- c(
                                                              pla,
                                                              sprintf("1 TRAN %s", self@names_alt[i]),
                                                              sprintf("2 LANG %s", names(self@names_alt)[i])
                                                            )
                                                          }
                                                          
                                                          if(length(self@lat_long) == 1){
                                                            pla <- c(
                                                              pla,
                                                              "1 MAP",
                                                              sprintf("2 LATI %s", self@lat),
                                                              sprintf("2 LONG %s", self@long)
                                                            )
                                                          }
                                                          
                                                          for (i in seq_along(self@external_ids)) {
                                                            pla <- c(
                                                              pla,
                                                              sprintf("1 EXID %s", self@external_ids[i]),
                                                              sprintf("2 TYPE %s", names(self@external_ids)[i])
                                                            )
                                                          }
                                                          
                                                          c(
                                                            pla,
                                                            lst_to_ged(self@notes) |> increase_level(by = 1),
                                                            sprintf("1 SNOTE %s", self@note_links)
                                                          )
                                                          
                                                        })
                             ),
                             
                             validator = function(self) {
                               c(
                                 chk_input_size(self@name, "@name", 1, 1, 1),
                                 chk_input_size(self@form, "@form", 0, 1, 1),
                                 chk_input_size(self@language, "@language", 0, 1),
                                 #TODO: language lookup
                                 chk_input_size(self@names_alt, "@names_alt", min_char = 1),
                                 chk_input_size(names(self@names_alt), "@names_alt names", length(self@names_alt), length(self@names_alt)),
                                 #TODO: language lookup
                                 chk_input_size(self@lat_long, "@lat_long", 0, 1),
                                 chk_input_pattern(self@lat_long, "@lat_long", "^[NS]\\d{1,2}(\\.\\d{1,6})? [EW]\\d{1,3}(\\.\\d{2,6})?$"),
                                 chk_input_size(self@external_ids, "@external_ids", min_char = 1),
                                 chk_input_size(names(self@external_ids), "@external_ids names", length(self@external_ids), length(self@external_ids)),
                                 #TODO: EXID and TYPE pattern
                                 chk_input_pattern(self@note_links, "@note_links", reg_xref(TRUE)),
                                 chk_input_S7classes(self@notes, "@notes", class_note)
                               )
                             }
)



#' @export
class_address <- S7::new_class("class_address",
                               properties = list(
                                 full = S7::class_character,
                                 local_address_lines = S7::class_character,
                                 city = S7::class_character,
                                 state = S7::class_character,
                                 postal_code = S7::class_character,
                                 country = S7::class_character,
                                 
                                 as_val = S7::new_property(
                                   S7::class_character,
                                   getter = function(self){
                                     paste(
                                       self@city,
                                       self@state,
                                       self@country,
                                       sep = ", "
                                     ) |>
                                       gsub(pattern = "(, ){2,}", replacement = ", ") |>
                                       sub(pattern = "^, ", replacement = "") |>
                                       sub(pattern = ", $", replacement = "")
                                   }),
                                 
                                 as_ged = S7::new_property(
                                   S7::class_character,
                                   getter = function(self){
                                     c(
                                       sprintf("0 ADDR %s", self@full),
                                       sprintf("1 %s %s", paste0("ADR", seq_along(self@local_address_lines)), self@local_address_lines),
                                       sprintf("1 CITY %s", self@city),
                                       sprintf("1 STAE %s", self@state),
                                       sprintf("1 POST %s", self@postal_code),
                                       sprintf("1 CTRY %s", self@country)
                                     )
                                   })
                               ),
                               
                               validator = function(self) {
                                 c(
                                   chk_input_size(self@local_address_lines, "@local_address_lines", 0, 3, 1, 60),
                                   chk_input_size(self@city, "@city", 0, 1, 1, 60),
                                   chk_input_size(self@state, "@state", 0, 1, 1, 60),
                                   chk_input_size(self@postal_code, "@postal_code", 0, 1, 1, 10),
                                   chk_input_size(self@country, "@country", 0, 1, 1, 60)
                                   
                                 )
                               }
)

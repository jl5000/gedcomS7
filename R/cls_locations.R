#' @include utils_at.R cls_validators.R
NULL

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
                               phon_names = S7::class_character,
                               rom_names = S7::class_character,
                               lat_long = S7::class_character,
                               note_links = S7::class_character,
                               notes = S7::class_character,
                               
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
                                                          
                                                          pla <- sprintf("0 PLAC %s", self@name)
                                                          
                                                          for (i in seq_along(self@phon_names)) {
                                                            pla <- c(
                                                              pla,
                                                              sprintf("1 FONE %s", self@phon_names[i]),
                                                              sprintf("2 TYPE %s", names(self@phon_names)[i])
                                                            )
                                                          }
                                                          
                                                          for (i in seq_along(self@rom_names)) {
                                                            pla <- c(
                                                              pla,
                                                              sprintf("1 ROMN %s", self@rom_names[i]),
                                                              sprintf("2 TYPE %s", names(self@rom_names)[i])
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
                                                          
                                                          c(
                                                            pla,
                                                            sprintf("1 NOTE %s", self@note_links),
                                                            sprintf("1 NOTE %s", self@notes)
                                                          )
                                                          
                                                        })
                             ),
                             
                             validator = function(self) {
                               c(
                                 chk_input_size(self@name, "@name", 1, 1, 1, 120),
                                 chk_input_size(self@phon_names, "@phon_names", 0, 10000, 1, 120),
                                 chk_input_size(names(self@phon_names), "@phon_names names", length(self@phon_names), length(self@phon_names), 5, 30),
                                 chk_input_size(self@rom_names, "@rom_names", 0, 10000, 1, 120),
                                 chk_input_size(names(self@rom_names), "@rom_names names", length(self@rom_names), length(self@rom_names), 5, 30),
                                 chk_input_size(self@lat_long, "@lat_long", 0, 1, 2+1+2, 10+1+11),
                                 chk_input_pattern(self@lat_long, "@lat_long", "^[NS]\\d{1,2}(\\.\\d{1,6})? [EW]\\d{1,3}(\\.\\d{2,6})?$"),
                                 chk_input_size(self@note_links, "@note_links", 0, 10000, 3, 22),
                                 chk_input_pattern(self@note_links, "@note_links", reg_xref(TRUE)),
                                 chk_input_size(self@notes, "@notes", 0, 10000, 1, 32767)
                               )
                             }
)


#' Create an Address object
#' 
#' @param local_address_lines A character vector containing up to three local address lines.
#' @param city The city of the address.
#' @param state The state/county of the address.
#' @param postal_code The postal code of the address.
#' @param country The country of the address.
#' @param phone_numbers A character vector containing up to three phone numbers.
#' @param emails A character vector containing up to three email addresses.
#' @param faxes A character vector containing up to three fax numbers.
#' @param web_pages A character vector containing up to three web pages.
#'
#' @return An S7 Address object.
#' @export
#' @name class_address
NULL
class_address <- S7::new_class("class_address",
                               properties = list(
                                 local_address_lines = S7::class_character,
                                 city = S7::class_character,
                                 state = S7::class_character,
                                 postal_code = S7::class_character,
                                 country = S7::class_character,
                                 phone_numbers = S7::class_character,
                                 emails = S7::class_character,
                                 faxes = S7::class_character,
                                 web_pages = S7::class_character,
                                 
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
                                     
                                     addr <- c(
                                       "0 ADDR",
                                       sprintf("1 %s %s", paste0("ADR", seq_along(self@local_address_lines)), self@local_address_lines),
                                       sprintf("1 CITY %s", self@city),
                                       sprintf("1 STAE %s", self@state),
                                       sprintf("1 POST %s", self@postal_code),
                                       sprintf("1 CTRY %s", self@country),
                                       sprintf("0 PHON %s", self@phone_numbers),
                                       sprintf("0 EMAIL %s", self@emails),
                                       sprintf("0 FAX %s", self@faxes),
                                       sprintf("0 WWW %s", self@web_pages)
                                     )
                                     
                                     if(length(addr) == 1) return(character())
                                     addr
                                   })
                               ),
                               
                               validator = function(self) {
                                 c(
                                   chk_input_size(self@local_address_lines, "@local_address_lines", 0, 3, 1, 60),
                                   chk_input_size(self@city, "@city", 0, 1, 1, 60),
                                   chk_input_size(self@state, "@state", 0, 1, 1, 60),
                                   chk_input_size(self@postal_code, "@postal_code", 0, 1, 1, 10),
                                   chk_input_size(self@country, "@country", 0, 1, 1, 60),
                                   chk_input_size(self@phone_numbers, "@phone_numbers", 0, 3, 1, 90),
                                   chk_input_size(self@emails, "@emails", 0, 3, 5, 120),
                                   chk_input_size(self@faxes, "@faxes", 0, 3, 5, 60),
                                   chk_input_size(self@web_pages, "@web_pages", 0, 3, 4, 2047)
                                 )
                               }
)

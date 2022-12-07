
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
#' @return An R7 Place object.
#' @export
#' @tests
#' expect_error(class_place())
#' expect_error(class_place(""))
#' expect_snapshot_value(class_place("here"), "serialize")
#' @name class_place
NULL
class_place <- R7::new_class("class_place",
                         properties = list(
                           name = R7::class_character,
                           phon_names = R7::class_character,
                           rom_names = R7::class_character,
                           lat_long = R7::class_character,
                           notes = R7::class_list,
                           
                           lat = R7::new_property(R7::class_character,
                                              getter = function(self){
                                                if(length(self@lat_long) == 1){
                                                  unlist(strsplit(self@lat_long, split = " "))[1]
                                                } else { character() }
                                              }),
                           long = R7::new_property(R7::class_character,
                                              getter = function(self){
                                                if(length(self@lat_long) == 1){
                                                  unlist(strsplit(self@lat_long, split = " "))[2]
                                                } else { character() }
                                              }),
                           
                           as_df = R7::new_property(R7::class_data.frame,
                                                getter = function(self){
                                                  
                                                  pla_df <- df_rows(level = 0, tag = "PLAC", value = self@name)
                                                  
                                                  for (i in seq_along(self@phon_names)) {
                                                    pla_df <- dplyr::bind_rows(
                                                      pla_df,
                                                      df_rows(level = 1, tag = "FONE", value = self@phon_names[i]),
                                                      df_rows(level = 2, tag = "TYPE", value = names(self@phon_names)[i])
                                                    )
                                                  }
                                                  
                                                  for (i in seq_along(self@rom_names)) {
                                                    pla_df <- dplyr::bind_rows(
                                                      pla_df,
                                                      df_rows(level = 1, tag = "ROMN", value = self@rom_names[i]),
                                                      df_rows(level = 2, tag = "TYPE", value = names(self@rom_names)[i])
                                                    )
                                                  }
                                                  
                                                  if(length(self@lat_long) == 1){
                                                    pla_df <- dplyr::bind_rows(
                                                      pla_df,
                                                      df_rows(level = 1, tag = "MAP", value = ""),
                                                      df_rows(level = 2, tag = "LATI", value = self@lat),
                                                      df_rows(level = 2, tag = "LONG", value = self@long)
                                                    )
                                                  }

                                                  dplyr::bind_rows(pla_df,
                                                                   lst_to_df(self@notes, level_inc = 1))
                                                  
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
                             chk_input_R7classes(self@notes, "@notes", class_note)
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
#' @return An R7 Address object.
#' @export
#' @name class_address
NULL
class_address <- R7::new_class("class_address",
                           properties = list(
                             local_address_lines = R7::class_character,
                             city = R7::class_character,
                             state = R7::class_character,
                             postal_code = R7::class_character,
                             country = R7::class_character,
                             phone_numbers = R7::class_character,
                             emails = R7::class_character,
                             faxes = R7::class_character,
                             web_pages = R7::class_character,
                             
                             as_string = R7::new_property(R7::class_character,
                                                      getter = function(self){
                                                        paste(
                                                          paste(self$local_address_lines, collapse = ", "),
                                                          self$city,
                                                          self$state,
                                                          self$country,
                                                          sep = ", "
                                                        ) |>
                                                          stringr::str_replace_all("(, ){2,}", ", ") |>
                                                          stringr::str_remove("^, ") |>
                                                          stringr::str_remove(", $")
                                                      }),
                             
                             as_df = R7::new_property(R7::class_data.frame,
                                                  getter = function(self){
                                                    
                                                    add_df <- dplyr::bind_rows(
                                                      df_rows(level = 0, tag = "ADDR", value = ""),
                                                      df_rows(level = 1, tag = paste0("ADR", seq_along(self@local_address_lines)), value = self@local_address_lines),
                                                      df_rows(level = 1, tag = "CITY", value = self@city),
                                                      df_rows(level = 1, tag = "STAE", value = self@state),
                                                      df_rows(level = 1, tag = "POST", value = self@postal_code),
                                                      df_rows(level = 1, tag = "CTRY", value = self@country),
                                                      df_rows(level = 0, tag = "PHON", value = self@phone_numbers),
                                                      df_rows(level = 0, tag = "EMAIL", value = self@emails),
                                                      df_rows(level = 0, tag = "FAX", value = self@faxes),
                                                      df_rows(level = 0, tag = "WWW", value = self@web_pages)
                                                    )
                                                    
                                                    if(nrow(add_df) == 1) return(NULL)
                                                    add_df
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

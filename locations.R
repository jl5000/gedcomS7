
class_place <- new_class("class_place",
                         properties = list(
                           name = class_character,
                           phonetic_names = class_character,
                           phonetic_methods = class_character,
                           romanised_names = class_character,
                           romanised_methods = class_character,
                           latitude = class_character, #TODO: x@latitude/longitude doesn't work without valid_eventually
                           longitude = class_character,
                           notes = class_list,
                           
                           as_df = new_property(class_data.frame,
                                                getter = function(self){
                                                  
                                                  pla_df <- tibble::tibble(level = 0, tag = "PLAC", value = self@name)
                                                  
                                                  for (i in seq_along(self@phonetic_names)) {
                                                    pla_df <- dplyr::bind_rows(
                                                      pla_df,
                                                      tibble::tibble(level = 1, tag = "FONE", value = self@phonetic_names[i]),
                                                      tibble::tibble(level = 2, tag = "TYPE", value = self@phonetic_methods[i])
                                                    )
                                                  }
                                                  
                                                  for (i in seq_along(self@romanised_names)) {
                                                    pla_df <- dplyr::bind_rows(
                                                      pla_df,
                                                      tibble::tibble(level = 1, tag = "ROMN", value = self@romanised_names[i]),
                                                      tibble::tibble(level = 2, tag = "TYPE", value = self@romanised_methods[i])
                                                    )
                                                  }
                                                  
                                                  if(length(self@latitude) == 1){
                                                    pla_df <- dplyr::bind_rows(
                                                      pla_df,
                                                      tibble::tibble(level = 1, tag = "MAP", value = ""),
                                                      tibble::tibble(level = 2, tag = "LATI", value = self@latitude),
                                                      tibble::tibble(level = 2, tag = "LONG", value = self@longitude)
                                                    )
                                                  }
                                                  
                                                  nts <- #purrr::map(self@notes, ~ .x@as_df) |>
                                                    dplyr::bind_rows()
                                                  
                                                  dplyr::bind_rows(pla_df, nts)
                                                  
                                                })
                         ),
                         
                         validator = function(self) {
                           c(
                             chk_input_size(self@name, "@name", 1, 1, 1, 120),
                             chk_input_size(self@phonetic_names, "@phonetic_names", 0, 10000, 1, 120),
                             chk_input_size(self@phonetic_methods, "@phonetic_methods", length(self@phonetic_names), length(self@phonetic_names), 5, 30),
                             chk_input_size(self@romanised_names, "@romanised_names", 0, 10000, 1, 120),
                             chk_input_size(self@romanised_methods, "@romanised_methods", length(self@romanised_names), length(self@romanised_names), 5, 30),
                             chk_input_size(self@latitude, "@latitude", 0, 1, 2, 10),
                             chk_input_size(self@longitude, "@longitude", length(self@latitude), length(self@latitude), 2, 11),
                             chk_input_R7classes(self@notes, "@notes", class_note)
                           )
                         }
)



class_address <- new_class("class_address",
                           properties = list(
                             local_address_lines = class_character,
                             city = class_character,
                             state = class_character,
                             postal_code = class_character,
                             country = class_character,
                             phone_numbers = class_character,
                             emails = class_character,
                             faxes = class_character,
                             web_pages = class_character,
                             
                             as_string = new_property(class_character,
                                                      getter = function(self){
                                                        full_add <- ""
                                                        if(length(self@local_address_lines) > 0)
                                                          full_add <- paste(self@local_address_lines, collapse = ", ")
                                                        if(length(self@city) > 0)
                                                          full_add <- paste(full_add, self@city, sep = ", ")
                                                        if(length(self@state) > 0)
                                                          full_add <- paste(full_add, self@state, sep = ", ")
                                                        if(length(self@postal_code) > 0)
                                                          full_add <- paste(full_add, self@postal_code, sep = ", ")
                                                        if(length(self@country) > 0)
                                                          full_add <- paste(full_add, self@country, sep = ". ")
                                                        
                                                        full_add
                                                      }),
                             
                             as_df = new_property(class_data.frame,
                                                  getter = function(self){
                                                    
                                                    add_df <- dplyr::bind_rows(
                                                      tibble::tibble(level = 0, tag = "ADDR", value = ""),
                                                      tibble::tibble(level = 1, tag = paste0("ADR", seq_along(self@local_address_lines)), value = self@local_address_lines),
                                                      tibble::tibble(level = 1, tag = "CITY", value = self@city),
                                                      tibble::tibble(level = 1, tag = "STAE", value = self@state),
                                                      tibble::tibble(level = 1, tag = "POST", value = self@postal_code),
                                                      tibble::tibble(level = 1, tag = "CTRY", value = self@country),
                                                      tibble::tibble(level = 0, tag = "PHON", value = self@phone_numbers),
                                                      tibble::tibble(level = 0, tag = "EMAIL", value = self@emails),
                                                      tibble::tibble(level = 0, tag = "FAX", value = self@faxes),
                                                      tibble::tibble(level = 0, tag = "WWW", value = self@web_pages)
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

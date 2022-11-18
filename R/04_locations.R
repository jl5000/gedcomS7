
# FINISHED
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
                                                  
                                                  pla_df <- tibble::tibble(level = 0, tag = "PLAC", value = self@name)
                                                  
                                                  for (i in seq_along(self@phon_names)) {
                                                    pla_df <- dplyr::bind_rows(
                                                      pla_df,
                                                      tibble::tibble(level = 1, tag = "FONE", value = self@phon_names[i]),
                                                      tibble::tibble(level = 2, tag = "TYPE", value = names(self@phon_names)[i])
                                                    )
                                                  }
                                                  
                                                  for (i in seq_along(self@rom_names)) {
                                                    pla_df <- dplyr::bind_rows(
                                                      pla_df,
                                                      tibble::tibble(level = 1, tag = "ROMN", value = self@rom_names[i]),
                                                      tibble::tibble(level = 2, tag = "TYPE", value = names(self@rom_names)[i])
                                                    )
                                                  }
                                                  
                                                  if(length(self@lat_long) == 1){
                                                    pla_df <- dplyr::bind_rows(
                                                      pla_df,
                                                      tibble::tibble(level = 1, tag = "MAP", value = ""),
                                                      tibble::tibble(level = 2, tag = "LATI", value = self@lat),
                                                      tibble::tibble(level = 2, tag = "LONG", value = self@long)
                                                    )
                                                  }
                                                  
                                                  nts <- purrr::map(self@notes, ~ .x@as_df) |>
                                                    dplyr::bind_rows()
                                                  
                                                  if(nrow(nts) > 0) nts <- dplyr::mutate(nts, level = level + 1)
                                                  
                                                  dplyr::bind_rows(pla_df, nts)
                                                  
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


# FINISHED
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
                             
                             as_df = R7::new_property(R7::class_data.frame,
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

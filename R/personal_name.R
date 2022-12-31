#' @include helpers.R validators.R
NULL

class_name_pieces <- R7::new_class("class_name_pieces", #abstract = TRUE,
                         properties = list(
                           prefix = R7::class_character,
                           given = R7::class_character,
                           nickname = R7::class_character,
                           surname_prefix = R7::class_character,
                           surname = R7::class_character,
                           suffix = R7::class_character,
                           note_links = R7::class_character,
                           notes = R7::class_character,
                           citations = R7::class_list
                         ),
                         validator = function(self) {
                           c(
                             chk_input_size(self@prefix, "@prefix", 0, 1, 1, 30),
                             chk_input_size(self@given, "@given", 0, 1, 1, 120),
                             chk_input_size(self@nickname,"@nickname", 0, 1, 1, 30),
                             chk_input_size(self@surname_prefix, "@surname_prefix", 0, 1, 1, 30),
                             chk_input_size(self@surname, "@surname", 0, 1, 1, 120),
                             chk_input_size(self@suffix, "@suffix", 0, 1, 1, 30),
                             chk_input_size(self@note_links, "@note_links", 0, 10000, 3, 22),
                             chk_input_pattern(self@note_links, "@note_links", reg_xref(TRUE)),
                             chk_input_size(self@notes, "@notes", 0, 10000, 1, 32767),
                             chk_input_R7classes(self@citations, "@citations", class_citation)
                           )
                         }
                         
)


class_name_info <- R7::new_class("class_name_info", parent = class_name_pieces,
                  properties = list(
                    full = R7::class_character,
                    type = R7::class_character,
                    
                    as_df = R7::new_property(R7::class_data.frame,
                                             getter = function(self){
                                               rbind(
                                                 df_rows(level = 0, tag = "NAME", value = self@full),
                                                 df_rows(level = 1, tag = "TYPE", value = self@type),
                                                 df_rows(level = 1, tag = "NPFX", value = self@prefix),
                                                 df_rows(level = 1, tag = "GIVN", value = self@given),
                                                 df_rows(level = 1, tag = "NICK", value = self@nickname),
                                                 df_rows(level = 1, tag = "SPFX", value = self@surname_prefix),
                                                 df_rows(level = 1, tag = "SURN", value = self@surname),
                                                 df_rows(level = 1, tag = "NSFX", value = self@suffix),
                                                 df_rows(level = 1, tag = "NOTE", value = self@note_links),
                                                 df_rows(level = 1, tag = "NOTE", value = self@notes),
                                                 lst_to_df(self@citations, level_inc = 1)
                                               )
                                             })
                  ),
                  
                  validator = function(self){
                    pieces_error <- NULL
                    if(length(self@prefix) + length(self@given) + length(self@nickname) +
                       length(self@surname_prefix) + length(self@surname) + length(self@suffix) == 0)
                      pieces_error <- "Names require at least one name piece (prefix/given/nickname/surname_prefix/surname/suffix)"
                    c(
                      chk_input_size(self@full, "@full", 1, 1, 1, 120),
                      # Relaxed type for main name, even though phon/rom need it
                      chk_input_size(self@type, "@type", 0, 1, 5, 30),
                      pieces_error
                    )
                  }
)


class_personal_name <- R7::new_class("class_personal_name",
                           properties = list(
                             name = class_name_info,
                             phon_names = R7::class_list,
                             rom_names = R7::class_list,
                             
                             as_df = R7::new_property(R7::class_data.frame,
                                                      getter = function(self){
                                                        
                                                        phon_df <- lst_to_df(self@phon_names, level_inc = 1)
                                                        if(!is.null(phon_df))
                                                          phon_df[tag == "NAME", tag:= "FONE"]
                                                        
                                                        rom_df <- lst_to_df(self@rom_names, level_inc = 1)
                                                        if(!is.null(rom_df))
                                                          rom_df[tag == "NAME", tag:= "ROMN"]
                                                        
                                                        rbind(
                                                          obj_to_df(self@name, level_inc = 0),
                                                          phon_df,
                                                          rom_df
                                                        )
                                                      })
                           ),
                           
                           validator = function(self){
                             type_error <- NULL
                             for(phon in self@phon_names){
                               if(length(phon@type) == 0)
                                 type_error <- "Every phonetic and romanised name variation requires a type"
                             }
                             for(rom in self@rom_names){
                               if(length(rom@type) == 0)
                                 type_error <- "Every phonetic and romanised name variation requires a type"
                             }

                             c(
                               chk_input_size(self@name, "@name", 1, 1),
                               chk_input_R7classes(self@phon_names, "@phon_names", class_name_info),
                               chk_input_R7classes(self@rom_names, "@rom_names", class_name_info),
                               type_error
                             )
                           }
)


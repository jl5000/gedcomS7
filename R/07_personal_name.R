
# FINISHED
class_name_pieces <- R7::new_class("class_name_pieces",
                         properties = list(
                           prefix = R7::class_character,
                           given = R7::class_character,
                           nickname = R7::class_character,
                           surname_prefix = R7::class_character,
                           surname = R7::class_character,
                           suffix = R7::class_character,
                           notes = R7::class_list,
                           citations = R7::class_list,
                           
                           as_df = R7::new_property(R7::class_data.frame,
                                                     getter = function(self){

                                                       rbind(
                                                         df_rows(level = 0, tag = "NPFX", value = self@prefix),
                                                         df_rows(level = 0, tag = "GIVN", value = self@given),
                                                         df_rows(level = 0, tag = "NICK", value = self@nickname),
                                                         df_rows(level = 0, tag = "SPFX", value = self@surname_prefix),
                                                         df_rows(level = 0, tag = "SURN", value = self@surname),
                                                         df_rows(level = 0, tag = "NSFX", value = self@suffix),
                                                         lst_to_df(self@notes, level_inc = 0),
                                                         lst_to_df(self@citations, level_inc = 0)
                                                       ) 
                                                       
                                                     })
                         ),
                         validator = function(self) {
                           c(
                             chk_input_size(self@prefix, "@prefix", 0, 1, 1, 30),
                             chk_input_size(self@given, "@given", 0, 1, 1, 120),
                             chk_input_size(self@nickname,"@nickname", 0, 1, 1, 30),
                             chk_input_size(self@surname_prefix, "@surname_prefix", 0, 1, 1, 30),
                             chk_input_size(self@surname, "@surname", 0, 1, 1, 120),
                             chk_input_size(self@suffix, "@suffix", 0, 1, 1, 30),
                             chk_input_R7classes(self@notes, "@notes", class_note),
                             chk_input_R7classes(self@citations, "@citations", class_citation)
                           )
                         }
                         
)

# FINISHED
class_name_info <- R7::new_class("class_name_info",
                  properties = list(
                    full = R7::class_character,
                    type = R7::class_character,
                    pieces = class_name_pieces,
                    
                    as_df = R7::new_property(R7::class_data.frame,
                                             getter = function(self){
                                               rbind(
                                                 df_rows(level = 0, tag = "NAME", value = self@full),
                                                 df_rows(level = 1, tag = "TYPE", value = self@type),
                                                 obj_to_df(self@pieces, level_inc = 1)
                                               )
                                             })
                  ),
                  
                  validator = function(self){
                    c(
                      chk_input_size(self@full, "@full", 1, 1, 1, 120),
                      # Relaxed type for main name, even though phon/rom need it
                      chk_input_size(self@type, "@type", 0, 1, 5, 30),
                      chk_input_size(self@pieces, "@pieces", 1, 1)
                    )
                  }
                  )

# FINISHED
class_personal_name <- R7::new_class("class_personal_name",
                           properties = list(
                             name = class_name_info,
                             phon_names = R7::class_list,
                             rom_names = R7::class_list,
                             
                             as_df = R7::new_property(R7::class_data.frame,
                                                      getter = function(self){
                                                        
                                                        phon_df <- lst_to_df(self@phon_names, level_inc = 1)
                                                        if(!is.null(phon_df))
                                                          phon_df <- dplyr::mutate(phon_df, tag = ifelse(tag == "NAME", "FONE", tag))
                                                        
                                                        rom_df <- lst_to_df(self@rom_names, level_inc = 1)
                                                        if(!is.null(rom_df))
                                                          rom_df <- dplyr::mutate(rom_df, tag = ifelse(tag == "NAME", "ROMN", tag))
                                                        
                                                        rbind(
                                                          obj_to_df(self@name, level_inc = 0),
                                                          phon_df,
                                                          rom_df
                                                        )
                                                      })
                           ),
                           
                           validator = function(self){
                             c(
                               chk_input_size(self@name, "@name", 1, 1),
                               chk_input_R7classes(self@phon_names, "@phon_names", class_name_info),
                               chk_input_R7classes(self@rom_names, "@rom_names", class_name_info)
                             )
                           }
)

# x=class_personal_name(
#   name = class_name_info(
#     full = "Jamie Lendrum",
#     type = "birth",
#     pieces = class_name_pieces(given = "Jamie", surname = "Lendrum")
#     ),
#   phon_names = list(
#     class_name_info(
#       full = "James Lendrum",
#       type = "nicks",
#       pieces = class_name_pieces(given = "James", surname = "Lendrum")
#     ),
#     class_name_info(
#       full = "Jaime Lendrum",
#       type = "spellig",
#       pieces = class_name_pieces(given = "Jaime", surname = "Lendrum")
#     )
#   ),
#   rom_names = list(
#     class_name_info(
#       full = "bum bum",
#       type = "bummm",
#       pieces = class_name_pieces(given = "bum", surname = "bum")
#     ),
#     class_name_info(
#       full= "bam bam",
#       type = "bammm",
#       pieces = class_name_pieces(given = "bam", surname = "bam")
#     )
#   )
#   )

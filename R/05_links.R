
# FINISHED
class_repository_citation <- R7::new_class("class_repository_citation",
                                           properties = list(
                                             xref = R7::class_character,
                                             source_call_number = R7::class_character,
                                             
                                             as_df = R7::new_property(R7::class_data.frame,
                                                                      getter = function(self){
                                                                        dplyr::bind_rows(
                                                                          df_rows(level = 0, tag = "REPO", value = self@xref),
                                                                          df_rows(level = 1, tag = "CALN", value = self@source_call_number)
                                                                        )
                                                                      })
                                           ),
                                           
                                           validator = function(self){
                                             c(
                                               chk_input_size(self@xref, "@xref", 1, 1, 3, 22),
                                               chk_input_pattern(self@xref, "@xref", reg_xref(TRUE)),
                                               chk_input_size(self@source_call_number, "@source_call_number", 0, 1, 1, 120)
                                             )
                                           }
)

# FINISHED
class_association <- R7::new_class("class_association",
                                   properties = list(
                                     xref = R7::class_character,
                                     relation_is = R7::class_character,
                                     citations = R7::class_list,
                                     notes = R7::class_list,
                                     
                                     as_df = R7::new_property(R7::class_data.frame,
                                                              getter = function(self){
                                                                dplyr::bind_rows(
                                                                  df_rows(level = 0, tag = "ASSO", value = self@xref),
                                                                  df_rows(level = 1, tag = "RELA", value = self@relation_is),
                                                                  lst_to_df(self@citations, level_inc = 1),
                                                                  lst_to_df(self@notes, level_inc = 1)
                                                                )
                                                              })
                                   ),
                                   
                                   validator = function(self){
                                     c(
                                       chk_input_size(self@xref, "@xref", 1, 1, 3, 22),
                                       chk_input_pattern(self@xref, "@xref", reg_xref(TRUE)),
                                       chk_input_size(self@relation_is, "@relation_is", 1, 1, 1, 25),
                                       chk_input_R7classes(self@citations, "@citations", class_citation),
                                       chk_input_R7classes(self@notes, "@notes", class_note)
                                     )
                                   }
)

# FINISHED
class_family_link <- R7::new_class("family_links", # TODO: abstract = TRUE,
                                properties = list(
                                  xref = R7::class_character,
                                  notes = R7::class_list
                                ),
                                
                                validator = function(self){
                                  c(
                                    chk_input_size(self@xref, "@xref", 1, 1, 3, 22),
                                    chk_input_pattern(self@xref, "@xref", reg_xref(TRUE)),
                                    chk_input_R7classes(self@notes, "@notes", class_note)
                                  )
                                }
)

# FINISHED
class_child_to_family_link <- R7::new_class("class_child_to_family_link", parent = class_family_link,
                                        properties = list(
                                          pedigree = R7::new_property(R7::class_character, default = "birth"),
                                          
                                          as_df = R7::new_property(R7::class_data.frame,
                                                                   getter = function(self){
                                                                     dplyr::bind_rows(
                                                                       df_rows(level = 0, tag = "FAMC", value = self@xref),
                                                                       df_rows(level = 1, tag = "PEDI", value = self@pedigree),
                                                                       lst_to_df(self@notes, level_inc = 1)
                                                                     )
                                                                   })
                                        ),
                                        
                                        validator = function(self){
                                          c(
                                            chk_input_size(self@pedigree, "@pedigree", 0, 1, 5, 7),
                                            chk_input_choice(self@pedigree, "@pedigree", val_pedigree_linkage_types())
                                          )
                                        }
)

# FINISHED
class_spouse_to_family_link <- R7::new_class("class_spouse_to_family_link", parent = class_family_link,
                                             properties = list(
                                               as_df = R7::new_property(R7::class_data.frame,
                                                                        getter = function(self){
                                                                          dplyr::bind_rows(
                                                                            df_rows(level = 0, tag = "FAMS", value = self@xref),
                                                                            lst_to_df(self@notes, level_inc = 1)
                                                                          )
                                                                        })
                                             )
)



class_event_detail <- R7::new_class("class_event_detail",
                                    properties = list(
                                      type = R7::class_character,
                                      date = R7::new_property(R7::new_union(NULL, 
                                                                            class_date_calendar,
                                                                            class_date_period,
                                                                            class_date_range,
                                                                            class_date_approx, 
                                                                            R7::class_character)),
                                      place = R7::new_property(R7::new_union(NULL, class_place)),
                                      address = R7::new_property(R7::new_union(NULL, class_address)),
                                      agency = R7::class_character,
                                      relig_affil = R7::class_character,
                                      cause = R7::class_character,
                                      note_links = R7::class_character,
                                      notes = R7::class_character,
                                      citations = R7::class_list,
                                      media_links = R7::class_character,
                                      
                                      event_date = R7::new_property(
                                        R7::class_character,
                                        getter = function(self){
                                          self@as_df[tag == "DATE",value]
                                        }),
                                      
                                      event_location = R7::new_property(
                                        R7::class_character,
                                        getter = function(self){
                                          if(length(self@place) == 1){
                                            self@place@as_val
                                          } else if(length(self@address) == 1) {
                                            self@address@as_val
                                          } else {
                                            character()
                                          }
                                        }),
                                      
                                      as_df = R7::new_property(
                                        R7::class_data.frame,
                                        getter = function(self){
                                          rbind(
                                            df_rows(level = 0, tag = "TYPE", value = self@type),
                                            date_to_df(self@date, level_inc = 0),
                                            obj_to_df(self@place, level_inc = 0),
                                            obj_to_df(self@address, level_inc = 0),
                                            df_rows(level = 0, tag = "AGNC", value = self@agency),
                                            df_rows(level = 0, tag = "RELI", value = self@relig_affil),
                                            df_rows(level = 0, tag = "CAUS", value = self@cause),
                                            df_rows(level = 0, tag = "NOTE", value = self@note_links),
                                            df_rows(level = 0, tag = "NOTE", value = self@notes),
                                            lst_to_df(self@citations, level_inc = 0),
                                            df_rows(level = 0, tag = "OBJE", value = self@media_links)
                                          )
                                        })
                                    ),
                                    validator = function(self) {
                                      c(
                                        chk_input_size(self@type, "@type", 0, 1, 1, 90),
                                        chk_input_size(self@date, "@date", 0, 1),
                                        chk_input_pattern(self@date, "@date", reg_date_value()),
                                        chk_input_size(self@place, "@place", 0, 1),
                                        chk_input_size(self@address, "@address", 0, 1),
                                        chk_input_size(self@agency, "@agency", 0, 1, 1, 120),
                                        chk_input_size(self@relig_affil, "@relig_affil", 0, 1, 1, 90),
                                        chk_input_size(self@cause, "@cause", 0, 1, 1, 90),
                                        chk_input_size(self@note_links, "@note_links", 0, 10000, 3, 22),
                                        chk_input_pattern(self@note_links, "@note_links", reg_xref(TRUE)),
                                        chk_input_size(self@notes, "@notes", 0, 10000, 1, 32767),
                                        chk_input_R7classes(self@citations, "@citations", class_citation),
                                        chk_input_size(self@media_links, "@media_links", 0, 10000, 3, 22),
                                        chk_input_pattern(self@media_links, "@media_links", reg_xref(TRUE))
                                      )
                                    }
)

class_famg_fact <- R7::new_class("class_famg_fact", #abstract = TRUE,
                                 properties = list(
                                   husband_age = R7::class_character,
                                   wife_age = R7::class_character,
                                   details = R7::new_property(R7::new_union(NULL, class_event_detail)),
                                   
                                   as_df_no_context = R7::new_property(
                                     R7::class_data.frame,
                                     getter = function(self){
                                       rbind(
                                         df_rows(level = 1, tag = "HUSB", value = rep("", length(self@husband_age))),
                                         df_rows(level = 2, tag = "AGE", value = self@husband_age),
                                         df_rows(level = 1, tag = "WIFE", value = rep("", length(self@wife_age))),
                                         df_rows(level = 2, tag = "AGE", value = self@wife_age),
                                         obj_to_df(self@details, level_inc = 1)
                                       )
                                     })
                                 ),
                                 validator = function(self) {
                                   c(
                                     chk_input_size(self@husband_age, "@husband_age", 0, 1, 2, 13),
                                     chk_input_size(self@wife_age, "@wife_age", 0, 1, 2, 13),
                                     chk_input_pattern(self@husband_age, "@husband_age", reg_age_at_event()),
                                     chk_input_pattern(self@wife_age, "@wife_age", reg_age_at_event()),
                                     chk_input_size(self@details, "@details", 0, 1)
                                   )
                                 }
)

class_indi_fact <- R7::new_class("class_indi_fact", #abstract = TRUE,
                                 properties = list(
                                   age = R7::class_character,
                                   details = R7::new_property(R7::new_union(NULL, class_event_detail)),
                                   
                                   as_df_no_context = R7::new_property(
                                     R7::class_data.frame,
                                     getter = function(self){
                                       rbind(
                                         df_rows(level = 1, tag = "AGE", value = self@age),
                                         obj_to_df(self@details, level_inc = 1)
                                       )
                                     })
                                 ),
                                 validator = function(self) {
                                   c(
                                     chk_input_size(self@age, "@age", 0, 1, 2, 13),
                                     chk_input_pattern(self@age, "@age", reg_age_at_event()),
                                     chk_input_size(self@details, "@details", 0, 1)
                                   )
                                 }
)

# Individual attributes
caste <- R7::new_class("caste", parent = class_indi_fact,
                       properties = list(
                         caste_name = R7::class_character,
                         
                         as_df = R7::new_property(
                           R7::class_data.frame,
                           getter = function(self){
                             rbind(
                               df_rows(level = 0, tag = "CAST", value = self@caste_name),
                               self@as_df_no_context
                             )
                           })
                       ),
                       validator = function(self){
                         c(
                           chk_input_size(self@caste_name, "@caste_name", 1, 1, 1, 90)
                         )
                       })

academic_achievement <- R7::new_class("academic_achievement", parent = class_indi_fact,
                                      properties = list(
                                        achievement = R7::class_character,
                                        
                                        as_df = R7::new_property(
                                          R7::class_data.frame,
                                          getter = function(self){
                                            rbind(
                                              df_rows(level = 0, tag = "EDUC", value = self@achievement),
                                              self@as_df_no_context
                                            )
                                          })
                                      ),
                                      validator = function(self){
                                        c(
                                          chk_input_size(self@achievement, "@achievement", 1, 1, 1, 248)
                                        )
                                      })

national_id_number <- R7::new_class("national_id_number", parent = class_indi_fact,
                                    properties = list(
                                      id_number = R7::class_character,
                                      
                                      as_df = R7::new_property(
                                        R7::class_data.frame,
                                        getter = function(self){
                                          rbind(
                                            df_rows(level = 0, tag = "IDNO", value = self@id_number),
                                            self@as_df_no_context
                                          )
                                        })
                                    ),
                                    validator = function(self){
                                      chk_type_exists <- NULL
                                      if(length(self@details) == 1)
                                        chk_type_exists <- chk_input_size(self@details@type, "@details@type", 1, 1)
                                      c(
                                        chk_input_size(self@id_number, "@id_number", 1, 1, 1, 30),
                                        chk_input_size(self@details, "@details", 1, 1),
                                        chk_type_exists
                                      )
                                    })

nationality <- R7::new_class("nationality", parent = class_indi_fact,
                             properties = list(
                               nat = R7::class_character,
                               
                               as_df = R7::new_property(
                                 R7::class_data.frame,
                                 getter = function(self){
                                   rbind(
                                     df_rows(level = 0, tag = "NATI", value = self@nat),
                                     self@as_df_no_context
                                   )
                                 })
                             ),
                             validator = function(self){
                               c(
                                 chk_input_size(self@nat, "@nat", 1, 1, 1, 120)
                               )
                             })

nobility_title <- R7::new_class("nobility_title", parent = class_indi_fact,
                                properties = list(
                                  title = R7::class_character,
                                  
                                  as_df = R7::new_property(
                                    R7::class_data.frame,
                                    getter = function(self){
                                      rbind(
                                        df_rows(level = 0, tag = "TITL", value = self@title),
                                        self@as_df_no_context
                                      )
                                    })
                                ),
                                validator = function(self){
                                  c(
                                    chk_input_size(self@title, "@title", 1, 1, 1, 120)
                                  )
                                })

num_children <- R7::new_class("num_children", parent = class_indi_fact,
                              properties = list(
                                num_chil = R7::class_integer,
                                
                                as_df = R7::new_property(
                                  R7::class_data.frame,
                                  getter = function(self){
                                    rbind(
                                      df_rows(level = 0, tag = "NCHI", value = as.character(self@num_chil)),
                                      self@as_df_no_context
                                    )
                                  })
                              ),
                              validator = function(self){
                                c(
                                  chk_input_size(self@num_chil, "@num_chil", 1, 1, 1, 3)
                                )
                              })

num_relationships <- R7::new_class("num_relationships", parent = class_indi_fact,
                                   properties = list(
                                     num_rel = R7::class_integer,
                                     
                                     as_df = R7::new_property(
                                       R7::class_data.frame,
                                       getter = function(self){
                                         rbind(
                                           df_rows(level = 0, tag = "NMR", value = as.character(self@num_rel)),
                                           self@as_df_no_context
                                         )
                                       })
                                   ),
                                   validator = function(self){
                                     c(
                                       chk_input_size(self@num_rel, "@num_rel", 1, 1, 1, 3)
                                     )
                                   })


occupation <- R7::new_class("occupation", parent = class_indi_fact,
                            properties = list(
                              occu = R7::class_character,
                              
                              as_df = R7::new_property(
                                R7::class_data.frame,
                                getter = function(self){
                                  rbind(
                                    df_rows(level = 0, tag = "OCCU", value = self@occu),
                                    self@as_df_no_context
                                  )
                                })
                            ),
                            validator = function(self){
                              c(
                                chk_input_size(self@occu, "@occu", 1, 1, 1, 90)
                              )
                            })

physical_desc <- R7::new_class("physical_desc", parent = class_indi_fact,
                               properties = list(
                                 phys_desc = R7::class_character,
                                 
                                 as_df = R7::new_property(
                                   R7::class_data.frame,
                                   getter = function(self){
                                     rbind(
                                       df_rows(level = 0, tag = "DSCR", value = self@phys_desc),
                                       self@as_df_no_context
                                     )
                                   })
                               ),
                               validator = function(self){
                                 c(
                                   chk_input_size(self@phys_desc, "@phys_desc", 1, 1, 1, 4095)
                                 )
                               })

property <- R7::new_class("property", parent = class_indi_fact,
                          properties = list(
                            prop = R7::class_character,
                            
                            as_df = R7::new_property(
                              R7::class_data.frame,
                              getter = function(self){
                                rbind(
                                  df_rows(level = 0, tag = "PROP", value = self@prop),
                                  self@as_df_no_context
                                )
                              })
                          ),
                          validator = function(self){
                            c(
                              chk_input_size(self@prop, "@prop", 1, 1, 1, 248)
                            )
                          })

religion <- R7::new_class("religion", parent = class_indi_fact,
                          properties = list(
                            relig = R7::class_character,
                            
                            as_df = R7::new_property(
                              R7::class_data.frame,
                              getter = function(self){
                                rbind(
                                  df_rows(level = 0, tag = "RELI", value = self@relig),
                                  self@as_df_no_context
                                )
                              })
                          ),
                          validator = function(self){
                            c(
                              chk_input_size(self@relig, "@relig", 1, 1, 1, 90)
                            )
                          })

residence <- R7::new_class("residence", parent = class_indi_fact,
                           properties = list(
                             as_df = R7::new_property(
                               R7::class_data.frame,
                               getter = function(self){
                                 rbind(
                                   df_rows(level = 0, tag = "RESI", value = ""),
                                   self@as_df_no_context
                                 )
                               })
                           ),
                           validator = function(self){
                             c(
                               chk_input_size(self@details, "@details", 1, 1)
                             )
                           })

indi_attribute <- R7::new_class("indi_attribute", parent = class_indi_fact,
                                properties = list(
                                  attr_descriptor = R7::class_character,
                                  
                                  as_df = R7::new_property(
                                    R7::class_data.frame,
                                    getter = function(self){
                                      rbind(
                                        df_rows(level = 0, tag = "FACT", value = self@attr_descriptor),
                                        self@as_df_no_context
                                      )
                                    })
                                ),
                                validator = function(self){
                                  chk_type_exists <- NULL
                                  if(length(self@details) == 1)
                                    chk_type_exists <- chk_input_size(self@details@type, "@details@type", 1, 1)
                                  c(
                                    chk_input_size(self@attr_descriptor, "@attr_descriptor", 1, 1, 1, 90),
                                    chk_input_size(self@details, "@details", 1, 1),
                                    chk_type_exists
                                  )
                                })


# Individual events
adoption <- R7::new_class("adoption", parent = class_indi_fact,
                          properties = list(
                            famg_xref = R7::class_character,
                            adopting_parent = R7::class_character,
                            
                            as_df = R7::new_property(
                              R7::class_data.frame,
                              getter = function(self){
                                rbind(
                                  df_rows(level = 0, tag = "ADOP", value = ""),
                                  df_rows(level = 1, tag = "FAMC", value = self@famg_xref),
                                  df_rows(level = 2, tag = "ADOP", value = rep(self@adopting_parent, 
                                                                               length(self@famg_xref))),
                                  self@as_df_no_context
                                )
                              })
                          ),
                          validator = function(self){
                            c(
                              chk_input_size(self@famg_xref, "@famg_xref", 0, 1, 3, 22),
                              chk_input_pattern(self@famg_xref, "@famg_xref", reg_xref(TRUE)),
                              chk_input_size(self@adopting_parent, "@adopting_parent", 0, 1, 4, 4),
                              chk_input_choice(self@adopting_parent, "@adopting_parent", val_adoptive_parents())
                            )
                          }
)

adult_christening <- R7::new_class("adult_christening", parent = class_indi_fact,
                                   properties = list(
                                     as_df = R7::new_property(
                                       R7::class_data.frame,
                                       getter = function(self){
                                         rbind(
                                           df_rows(level = 0, tag = "CHRA", value = ""),
                                           self@as_df_no_context
                                         )
                                       })))
baptism <- R7::new_class("baptism", parent = class_indi_fact,
                         properties = list(
                           as_df = R7::new_property(
                             R7::class_data.frame,
                             getter = function(self){
                               rbind(
                                 df_rows(level = 0, tag = "BAPM", value = ""),
                                 self@as_df_no_context
                               )
                             })))
bar_mitzvah <- R7::new_class("bar_mitzvah", parent = class_indi_fact,
                             properties = list(
                               as_df = R7::new_property(
                                 R7::class_data.frame,
                                 getter = function(self){
                                   rbind(
                                     df_rows(level = 0, tag = "BARM", value = ""),
                                     self@as_df_no_context
                                   )
                                 })))
bas_mitzvah <- R7::new_class("bas_mitzvah", parent = class_indi_fact,
                             properties = list(
                               as_df = R7::new_property(
                                 R7::class_data.frame,
                                 getter = function(self){
                                   rbind(
                                     df_rows(level = 0, tag = "BASM", value = ""),
                                     self@as_df_no_context
                                   )
                                 })))
birth <- R7::new_class("birth", parent = class_indi_fact,
                       properties = list(
                         famg_xref = R7::class_character,
                         
                         as_df = R7::new_property(
                           R7::class_data.frame,
                           getter = function(self){
                             rbind(
                               df_rows(level = 0, tag = "BIRT", value = ""),
                               df_rows(level = 1, tag = "FAMC", value = self@famg_xref),
                               self@as_df_no_context
                             )
                           })
                       ),
                       validator = function(self){
                         c(
                           chk_input_size(self@famg_xref, "@famg_xref", 0, 1, 3, 22),
                           chk_input_pattern(self@famg_xref, "@famg_xref", reg_xref(TRUE))
                         )
                       }
)
burial <- R7::new_class("burial", parent = class_indi_fact,
                        properties = list(
                          as_df = R7::new_property(
                            R7::class_data.frame,
                            getter = function(self){
                              rbind(
                                df_rows(level = 0, tag = "BURI", value = ""),
                                self@as_df_no_context
                              )
                            })))
census <- R7::new_class("census", parent = class_indi_fact,
                        properties = list(
                          as_df = R7::new_property(
                            R7::class_data.frame,
                            getter = function(self){
                              rbind(
                                df_rows(level = 0, tag = "CENS", value = ""),
                                self@as_df_no_context
                              )
                            })))
christening <- R7::new_class("christening", parent = class_indi_fact,
                             properties = list(
                               famg_xref = R7::class_character,
                               
                               as_df = R7::new_property(
                                 R7::class_data.frame,
                                 getter = function(self){
                                   rbind(
                                     df_rows(level = 0, tag = "CHR", value = ""),
                                     df_rows(level = 1, tag = "FAMC", value = self@famg_xref),
                                     self@as_df_no_context
                                   )
                                 })
                             ),
                             validator = function(self){
                               c(
                                 chk_input_size(self@famg_xref, "@famg_xref", 0, 1, 3, 22),
                                 chk_input_pattern(self@famg_xref, "@famg_xref", reg_xref(TRUE))
                               )
                             }
)
confirmation <- R7::new_class("confirmation", parent = class_indi_fact,
                              properties = list(
                                as_df = R7::new_property(
                                  R7::class_data.frame,
                                  getter = function(self){
                                    rbind(
                                      df_rows(level = 0, tag = "CONF", value = ""),
                                      self@as_df_no_context
                                    )
                                  })))
cremation <- R7::new_class("cremation", parent = class_indi_fact,
                           properties = list(
                             as_df = R7::new_property(
                               R7::class_data.frame,
                               getter = function(self){
                                 rbind(
                                   df_rows(level = 0, tag = "CREM", value = ""),
                                   self@as_df_no_context
                                 )
                               })))
death <- R7::new_class("death", parent = class_indi_fact,
                       properties = list(
                         as_df = R7::new_property(
                           R7::class_data.frame,
                           getter = function(self){
                             rbind(
                               df_rows(level = 0, tag = "DEAT", value = ""),
                               self@as_df_no_context
                             )
                           })))
emigration <- R7::new_class("emigration", parent = class_indi_fact,
                            properties = list(
                              as_df = R7::new_property(
                                R7::class_data.frame,
                                getter = function(self){
                                  rbind(
                                    df_rows(level = 0, tag = "EMIG", value = ""),
                                    self@as_df_no_context
                                  )
                                })))
first_communion <- R7::new_class("first_communion", parent = class_indi_fact,
                                 properties = list(
                                   as_df = R7::new_property(
                                     R7::class_data.frame,
                                     getter = function(self){
                                       rbind(
                                         df_rows(level = 0, tag = "FCOM", value = ""),
                                         self@as_df_no_context
                                       )
                                     })))
graduation <- R7::new_class("graduation", parent = class_indi_fact,
                            properties = list(
                              as_df = R7::new_property(
                                R7::class_data.frame,
                                getter = function(self){
                                  rbind(
                                    df_rows(level = 0, tag = "GRAD", value = ""),
                                    self@as_df_no_context
                                  )
                                })))
immigration <- R7::new_class("immigration", parent = class_indi_fact,
                             properties = list(
                               as_df = R7::new_property(
                                 R7::class_data.frame,
                                 getter = function(self){
                                   rbind(
                                     df_rows(level = 0, tag = "IMMI", value = ""),
                                     self@as_df_no_context
                                   )
                                 })))
naturalization <- R7::new_class("naturalization", parent = class_indi_fact,
                                properties = list(
                                  as_df = R7::new_property(
                                    R7::class_data.frame,
                                    getter = function(self){
                                      rbind(
                                        df_rows(level = 0, tag = "NATU", value = ""),
                                        self@as_df_no_context
                                      )
                                    })))
probate <- R7::new_class("probate", parent = class_indi_fact,
                         properties = list(
                           as_df = R7::new_property(
                             R7::class_data.frame,
                             getter = function(self){
                               rbind(
                                 df_rows(level = 0, tag = "PROB", value = ""),
                                 self@as_df_no_context
                               )
                             })))
retirement <- R7::new_class("retirement", parent = class_indi_fact,
                            properties = list(
                              as_df = R7::new_property(
                                R7::class_data.frame,
                                getter = function(self){
                                  rbind(
                                    df_rows(level = 0, tag = "RETI", value = ""),
                                    self@as_df_no_context
                                  )
                                })))
will <- R7::new_class("will", parent = class_indi_fact,
                      properties = list(
                        as_df = R7::new_property(
                          R7::class_data.frame,
                          getter = function(self){
                            rbind(
                              df_rows(level = 0, tag = "WILL", value = ""),
                              self@as_df_no_context
                            )
                          })))
indi_event <- R7::new_class("indi_event", parent = class_indi_fact,
                            properties = list(
                              event_descriptor = R7::class_character,
                              
                              as_df = R7::new_property(
                                R7::class_data.frame,
                                getter = function(self){
                                  rbind(
                                    df_rows(level = 0, tag = "EVEN", value = self@event_descriptor),
                                    self@as_df_no_context
                                  )
                                })
                            ),
                            validator = function(self){
                              c(
                                chk_input_size(self@event_descriptor, "@event_descriptor", 0, 1, 1, 90)
                              )
                            })

# Family events
annulment <- R7::new_class("annulment", parent = class_famg_fact,
                           properties = list(
                             as_df = R7::new_property(
                               R7::class_data.frame,
                               getter = function(self){
                                 rbind(
                                   df_rows(level = 0, tag = "ANUL", value = ""),
                                   self@as_df_no_context
                                 )
                               })))
divorce <- R7::new_class("divorce", parent = class_famg_fact,
                         properties = list(
                           as_df = R7::new_property(
                             R7::class_data.frame,
                             getter = function(self){
                               rbind(
                                 df_rows(level = 0, tag = "DIV", value = ""),
                                 self@as_df_no_context
                               )
                             })))
divorce_filed <- R7::new_class("divorce_filed", parent = class_famg_fact,
                               properties = list(
                                 as_df = R7::new_property(
                                   R7::class_data.frame,
                                   getter = function(self){
                                     rbind(
                                       df_rows(level = 0, tag = "DIVF", value = ""),
                                       self@as_df_no_context
                                     )
                                   })))
engagement <- R7::new_class("engagement", parent = class_famg_fact,
                            properties = list(
                              as_df = R7::new_property(
                                R7::class_data.frame,
                                getter = function(self){
                                  rbind(
                                    df_rows(level = 0, tag = "ENGA", value = ""),
                                    self@as_df_no_context
                                  )
                                })))
marr_banns <- R7::new_class("marr_banns", parent = class_famg_fact,
                            properties = list(
                              as_df = R7::new_property(
                                R7::class_data.frame,
                                getter = function(self){
                                  rbind(
                                    df_rows(level = 0, tag = "MARB", value = ""),
                                    self@as_df_no_context
                                  )
                                })))
marr_contract <- R7::new_class("marr_contract", parent = class_famg_fact,
                               properties = list(
                                 as_df = R7::new_property(
                                   R7::class_data.frame,
                                   getter = function(self){
                                     rbind(
                                       df_rows(level = 0, tag = "MARC", value = ""),
                                       self@as_df_no_context
                                     )
                                   })))
marr_license <- R7::new_class("marr_license", parent = class_famg_fact,
                              properties = list(
                                as_df = R7::new_property(
                                  R7::class_data.frame,
                                  getter = function(self){
                                    rbind(
                                      df_rows(level = 0, tag = "MARL", value = ""),
                                      self@as_df_no_context
                                    )
                                  })))
marr_settlement <- R7::new_class("marr_settlement", parent = class_famg_fact,
                                 properties = list(
                                   as_df = R7::new_property(
                                     R7::class_data.frame,
                                     getter = function(self){
                                       rbind(
                                         df_rows(level = 0, tag = "MARS", value = ""),
                                         self@as_df_no_context
                                       )
                                     })))
relationship <- R7::new_class("relationship", parent = class_famg_fact,
                              properties = list(
                                as_df = R7::new_property(
                                  R7::class_data.frame,
                                  getter = function(self){
                                    rbind(
                                      df_rows(level = 0, tag = "MARR", value = ""),
                                      self@as_df_no_context
                                    )
                                  })))
famg_event <- R7::new_class("will", parent = class_famg_fact,
                            properties = list(
                              event_descriptor = R7::class_character,
                              
                              as_df = R7::new_property(
                                R7::class_data.frame,
                                getter = function(self){
                                  rbind(
                                    df_rows(level = 0, tag = "EVEN", value = self@event_descriptor),
                                    self@as_df_no_context
                                  )
                                })
                            ),
                            validator = function(self){
                              c(
                                chk_input_size(self@event_descriptor, "@event_descriptor", 1, 1, 1, 90)
                              )
                            })


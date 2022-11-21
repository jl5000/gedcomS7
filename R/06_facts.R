
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
                                      notes = R7::class_list,
                                      citations = R7::class_list,
                                      media_links = R7::class_list,
                                      
                                      as_df = R7::new_property(R7::class_data.frame,
                                                               getter = function(self){
                                                                 dplyr::bind_rows(
                                                                   df_rows(level = 0, tag = "TYPE", value = self@type),
                                                                   date_to_df(self@date, level_inc = 0),
                                                                   obj_to_df(self@place, level_inc = 0),
                                                                   obj_to_df(self@address, level_inc = 0),
                                                                   df_rows(level = 0, tag = "AGNC", value = self@agency),
                                                                   df_rows(level = 0, tag = "RELI", value = self@relig_affil),
                                                                   df_rows(level = 0, tag = "CAUS", value = self@cause),
                                                                   lst_to_df(self@notes, level_inc = 0),
                                                                   lst_to_df(self@citations, level_inc = 0),
                                                                   lst_to_df(self@media_links, level_inc = 0)
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
                                        chk_input_R7classes(self@notes, "@notes", class_note),
                                        chk_input_R7classes(self@citations, "@notes", class_citation),
                                        chk_input_R7classes(self@media_links, "@notes", class_media_link)
                                      )
                                    }
)

class_famg_fact <- R7::new_class("class_famg_fact", #TODO: abstract = TRUE,
                                 properties = list(
                                   husband_age = R7::class_character,
                                   wife_age = R7::class_character,
                                   fact_detail = R7::new_property(R7::new_union(NULL, class_event_detail)),
                                   
                                   as_df_no_context = R7::new_property(R7::class_data.frame,
                                                                       getter = function(self){
                                                                         dplyr::bind_rows(
                                                                           df_rows(level = 1, tag = "HUSB", value = rep("", length(self@husband_age))),
                                                                           df_rows(level = 2, tag = "AGE", value = self@husband_age),
                                                                           df_rows(level = 1, tag = "WIFE", value = rep("", length(self@wife_age))),
                                                                           df_rows(level = 2, tag = "AGE", value = self@wife_age),
                                                                           obj_to_df(self@fact_detail, level_inc = 1)
                                                                         )
                                                                       })
                                 ),
                                 validator = function(self) {
                                   c(
                                     chk_input_size(self@husband_age, "@husband_age", 0, 1, 2, 13),
                                     chk_input_size(self@wife_age, "@wife_age", 0, 1, 2, 13),
                                     chk_input_pattern(self@husband_age, "@husband_age", reg_age_at_event()),
                                     chk_input_pattern(self@wife_age, "@wife_age", reg_age_at_event()),
                                     chk_input_size(self@fact_detail, "@fact_detail", 0, 1)
                                   )
                                 }
)

class_indi_fact <- R7::new_class("class_indi_fact", #TODO: abstract = TRUE,
                                 properties = list(
                                   age = R7::class_character,
                                   fact_detail = R7::new_property(R7::new_union(NULL, class_event_detail)),
                                   
                                   as_df_no_context = R7::new_property(R7::class_data.frame,
                                                                       getter = function(self){
                                                                         dplyr::bind_rows(
                                                                           df_rows(level = 1, tag = "AGE", value = self@age),
                                                                           obj_to_df(self@fact_detail, level_inc = 1)
                                                                         )
                                                                       })
                                 ),
                                 validator = function(self) {
                                   c(
                                     chk_input_size(self@age, "@age", 0, 1, 2, 13),
                                     chk_input_pattern(self@age, "@age", reg_age_at_event()),
                                     chk_input_size(self@fact_detail, "@fact_detail", 0, 1)
                                   )
                                 }
)

# Individual attributes
caste <- R7::new_class("caste", parent = class_indi_fact,
                       properties = list(
                         caste_name = R7::class_character,
                         
                         as_df = R7::new_property(R7::class_data.frame,
                                                  getter = function(self){
                                                    dplyr::bind_rows(
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
                                        
                                        as_df = R7::new_property(R7::class_data.frame,
                                                                 getter = function(self){
                                                                   dplyr::bind_rows(
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
                                      
                                      as_df = R7::new_property(R7::class_data.frame,
                                                               getter = function(self){
                                                                 dplyr::bind_rows(
                                                                   df_rows(level = 0, tag = "IDNO", value = self@id_number),
                                                                   self@as_df_no_context
                                                                 )
                                                               })
                                      ),
                                    validator = function(self){
                                      chk_type_exists <- NULL
                                      if(length(self@fact_detail) == 1)
                                        chk_type_exists <- chk_input_size(self@fact_detail@type, "@fact_detail@type", 1, 1)
                                      c(
                                        chk_input_size(self@id_number, "@id_number", 1, 1, 1, 30),
                                        chk_input_size(self@fact_detail, "@fact_detail", 1, 1),
                                        chk_type_exists
                                      )
                                    })

nationality <- R7::new_class("nationality", parent = class_indi_fact,
                             properties = list(
                               nat = R7::class_character,
                               
                               as_df = R7::new_property(R7::class_data.frame,
                                                        getter = function(self){
                                                          dplyr::bind_rows(
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
                                  
                                  as_df = R7::new_property(R7::class_data.frame,
                                                           getter = function(self){
                                                             dplyr::bind_rows(
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
                                
                                as_df = R7::new_property(R7::class_data.frame,
                                                         getter = function(self){
                                                           dplyr::bind_rows(
                                                             df_rows(level = 0, tag = "NCHI", value = self@num_chil),
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
                                     
                                     as_df = R7::new_property(R7::class_data.frame,
                                                              getter = function(self){
                                                                dplyr::bind_rows(
                                                                  df_rows(level = 0, tag = "NMR", value = self@num_rel),
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
                              
                              as_df = R7::new_property(R7::class_data.frame,
                                                       getter = function(self){
                                                         dplyr::bind_rows(
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
                                 
                                 as_df = R7::new_property(R7::class_data.frame,
                                                          getter = function(self){
                                                            dplyr::bind_rows(
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
                            
                            as_df = R7::new_property(R7::class_data.frame,
                                                     getter = function(self){
                                                       dplyr::bind_rows(
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
                            
                            as_df = R7::new_property(R7::class_data.frame,
                                                     getter = function(self){
                                                       dplyr::bind_rows(
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
                             as_df = R7::new_property(R7::class_data.frame,
                                                      getter = function(self){
                                                        dplyr::bind_rows(
                                                          df_rows(level = 0, tag = "RESI", value = ""),
                                                          self@as_df_no_context
                                                        )
                                                      })
                           ),
                           validator = function(self){
                             c(
                               chk_input_size(self@fact_detail, "@fact_detail", 1, 1)
                             )
                           })

indi_attribute <- R7::new_class("indi_attribute", parent = class_indi_fact,
                                properties = list(
                                  attr_descriptor = R7::class_character,
                                  
                                  as_df = R7::new_property(R7::class_data.frame,
                                                           getter = function(self){
                                                             dplyr::bind_rows(
                                                               df_rows(level = 0, tag = "FACT", value = self@attr_descriptor),
                                                               self@as_df_no_context
                                                             )
                                                           })
                                ),
                                validator = function(self){
                                  chk_type_exists <- NULL
                                  if(length(self@fact_detail) == 1)
                                    chk_type_exists <- chk_input_size(self@fact_detail@type, "@fact_detail@type", 1, 1)
                                  c(
                                    chk_input_size(self@attr_descriptor, "@attr_descriptor", 1, 1, 1, 90),
                                    chk_input_size(self@fact_detail, "@fact_detail", 1, 1),
                                    chk_type_exists
                                  )
                                })


# Individual events
adoption <- R7::new_class("adoption", parent = class_indi_fact,
                          properties = list(
                            famc_xref = R7::class_character,
                            adopting_parent = R7::class_character
                          ))
adult_christening <- R7::new_class("adult_christening", parent = class_indi_fact)
baptism <- R7::new_class("baptism", parent = class_indi_fact)
bar_mitzvah <- R7::new_class("bar_mitzvah", parent = class_indi_fact)
bas_mitzvah <- R7::new_class("bas_mitzvah", parent = class_indi_fact)
birth <- R7::new_class("birth", parent = class_indi_fact,
                       properties = list(famc_xref = R7::class_character))
burial <- R7::new_class("burial", parent = class_indi_fact)
census <- R7::new_class("census", parent = class_indi_fact)
christening <- R7::new_class("christening", parent = class_indi_fact)
confirmation <- R7::new_class("confirmation", parent = class_indi_fact)
cremation <- R7::new_class("cremation", parent = class_indi_fact)
death <- R7::new_class("death", parent = class_indi_fact)
emigration <- R7::new_class("emigration", parent = class_indi_fact)
first_communion <- R7::new_class("first_communion", parent = class_indi_fact)
graduation <- R7::new_class("graduation", parent = class_indi_fact)
immigration <- R7::new_class("immigration", parent = class_indi_fact)
naturalization <- R7::new_class("naturalization", parent = class_indi_fact)
probate <- R7::new_class("probate", parent = class_indi_fact)
retirement <- R7::new_class("retirement", parent = class_indi_fact)
will <- R7::new_class("will", parent = class_indi_fact)
indi_event <- R7::new_class("indi_event", parent = class_indi_fact,
                            properties = list(event_descriptor = R7::class_character))

# Family events
annulment <- R7::new_class("annulment", parent = class_famg_fact,
                           properties = list(
                             as_df = R7::new_property(R7::class_data.frame,
                                                      getter = function(self){
                                                        dplyr::bind_rows(
                                                          df_rows(level = 0, tag = "ANUL", value = ""),
                                                          self@as_df_no_context
                                                        )
                                                      })
                           ))
divorce <- R7::new_class("divorce", parent = class_famg_fact)
divorce_filed <- R7::new_class("divorce_filed", parent = class_famg_fact)
engagement <- R7::new_class("engagement", parent = class_famg_fact)
marr_banns <- R7::new_class("marr_banns", parent = class_famg_fact)
marr_contract <- R7::new_class("marr_contract", parent = class_famg_fact)
marr_license <- R7::new_class("marr_license", parent = class_famg_fact)
marr_settlement <- R7::new_class("marr_settlement", parent = class_famg_fact)
relationship <- R7::new_class("relationship", parent = class_famg_fact)
famg_event <- R7::new_class("will", parent = class_famg_fact,
                            properties = list(event_descriptor = R7::class_character))


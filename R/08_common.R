

class_change_date <- R7::new_class("class_change_date",
                               properties = list(
                                 change_date = R7::new_property(R7::new_union(class_date_exact, R7::class_character), 
                                                            default = date_exact_current()),
                                 change_time = R7::class_character,
                                 notes = R7::class_list,
                                 
                                 as_df = R7::new_property(R7::class_data.frame,
                                                      getter = function(self){
                                                        
                                                        dplyr::bind_rows(
                                                          df_rows(level = 0, tag = "CHAN", value = ""),
                                                          date_to_df(self@change_date, level_inc = 1),
                                                          df_rows(level = 2, tag = "TIME", value = self@change_time),
                                                          lst_to_df(self@notes, level_inc = 1)
                                                        )

                                                      })
                               ),
                               validator = function(self) {
                                 c(
                                   chk_input_size(self@change_date, "@change_date", 1, 1),
                                   chk_input_pattern(self@change_date, "@change_date", reg_date_exact()),
                                   chk_input_size(self@change_time, "@change_time", 0, 1, 7, 12),
                                   chk_input_R7classes(self@notes, "@notes", class_note)
                                 )
                               }
)




# FINISHED
class_note <- R7::new_class("class_note",
                   properties = list(
                     xref = R7::class_character,
                     text = R7::class_character,
                     
                     as_df = R7::new_property(R7::class_data.frame,
                                          getter = function(self){
                                            if(length(self@xref) == 1){
                                              df_rows(level = 0, tag = "NOTE", value = self@xref)
                                            } else {
                                              df_rows(level = 0, tag = "NOTE", value = self@text)
                                            }
                                          })
                   ),
                   validator = function(self) {
                     c(
                       chk_input_size(self@xref, "@xref", 0, 1, 3, 22),
                       chk_input_size(self@text, "@text", 0, 1, 1, 32767),
                       chk_input_size(c(self@xref, self@text), "note", 1, 1),
                       chk_input_pattern(self@xref, "@xref", reg_xref(TRUE))
                     )
                   }
)



class_citation <- R7::new_class("class_citation",
                                properties = list(
                                  xref = R7::class_character,
                                  where = R7::class_character,
                                  event_type = R7::class_character,
                                  event_role = R7::class_character,
                                  recording_date = R7::new_property(R7::new_union(NULL, 
                                                                                  class_date_calendar,
                                                                                  class_date_period,
                                                                                  class_date_range,
                                                                                  class_date_approx, 
                                                                                  R7::class_character)),
                                  source_text = R7::class_character,
                                  media_links = R7::class_list,
                                  notes = R7::class_list,
                                  certainty = R7::class_character,
                                  
                                  as_df = R7::new_property(R7::class_data.frame,
                                                            getter = function(self){
                                                            
                                                              cit_df <- dplyr::bind_rows(
                                                                df_rows(level = 0, tag = "SOUR", value = self@xref),
                                                                df_rows(level = 1, tag = "PAGE", value = self@where),
                                                                df_rows(level = 1, tag = "EVEN", value = self@event_type),
                                                                df_rows(level = 2, tag = "ROLE", value = self@event_role),
                                                                df_rows(level = 1, tag = "DATA", value = ""),
                                                                date_to_df(self@recording_date, level_inc = 2),
                                                                df_rows(level = 2, tag = "TEXT", value = self@source_text),
                                                                lst_to_df(self@media_links, level_inc = 1),
                                                                lst_to_df(self@notes, level_inc = 1),
                                                                df_rows(level = 1, tag = "QUAY", value = self@certainty)
                                                              ) 
                                                              
                                                              if (sum(cit_df$tag == "EVEN") == 0) cit_df <- dplyr::filter(cit_df, tag != "ROLE")
                                                              if (sum(cit_df$tag == "DATE") == 0 & sum(cit_df$tag == "TEXT") == 0) 
                                                                cit_df <- dplyr::filter(cit_df, tag != "DATA")
                                                              
                                                              cit_df
                                                            })
                                ),
                                
                                validator = function(self){
                                  c(
                                    chk_input_size(self@xref, "@xref", 1, 1, 3, 22),
                                    chk_input_pattern(self@xref, "@xref", reg_xref(TRUE)),
                                    chk_input_size(self@where, "@where", 0, 1, 1, 248),
                                    chk_input_size(self@event_type, "@event_type", 0, 1, 1, 15),
                                    chk_input_size(self@event_role, "@event_role", 0, 1, 3, 27),
                                    chk_input_pattern(self@event_role, "@event_role",
                                                      paste(reg_role_in_event(), reg_custom_value(), sep = "|")),
                                    chk_input_size(self@recording_date, "@recording_date", 0, 1),
                                    chk_input_pattern(self@recording_date, "@recording_date", reg_date_value()),
                                    chk_input_size(self@source_text, "@source_text", 0, 10000, 1, 32767),
                                    chk_input_R7classes(self@media_links, "@media_links", class_media_link),
                                    chk_input_R7classes(self@notes, "@notes", class_note),
                                    chk_input_size(self@certainty, "@certainty", 0, 1),
                                    chk_input_choice(self@certainty, "@certainty", as.character(0:3))
                                  )
                                }
                                
)

# FINISHED
class_media_link <- R7::new_class("class_media_link",
                                  properties = list(
                                    xref = R7::class_character,
                                    
                                    as_df = R7::new_property(R7::class_data.frame,
                                                             getter = function(self){
                                                               df_rows(level = 0, tag = "OBJE", value = self@xref)
                                                             })
                                  ),
                                  validator = function(self) {
                                    c(
                                      chk_input_size(self@xref, "@xref", 1, 1, 3, 22),
                                      chk_input_pattern(self@xref, "@xref", reg_xref(TRUE))
                                    )
                                  }
)

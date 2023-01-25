#' @include helpers.R cls_dates.R validators.R
NULL

class_change_date <- R7::new_class("class_change_date",
                                   properties = list(
                                     date = R7::new_property(R7::new_union(class_date_exact, R7::class_character), 
                                                             default = date_exact_current()),
                                     time = R7::class_character,
                                     note_links = R7::class_character,
                                     notes = R7::class_character,
                                     
                                     as_ged = R7::new_property(
                                       R7::class_character,
                                       getter = function(self){
                                         c(
                                           "0 CHAN",
                                           sprintf("1 DATE %s", date_to_val(self@date)),
                                           sprintf("2 TIME %s", self@time),
                                           sprintf("1 NOTE %s", self@note_links),
                                           sprintf("1 NOTE %s", self@notes)
                                         )
                                         
                                       })
                                   ),
                                   validator = function(self) {
                                     c(
                                       chk_input_size(self@date, "@date", 1, 1),
                                       chk_input_pattern(self@date, "@date", reg_date_exact()),
                                       chk_input_size(self@time, "@time", 0, 1, 4, 11), # different from spec
                                       chk_input_pattern(self@time, "@time", reg_time()),
                                       chk_input_size(self@note_links, "@note_links", 0, 10000, 3, 22),
                                       chk_input_pattern(self@note_links, "@note_links", reg_xref(TRUE)),
                                       chk_input_size(self@notes, "@notes", 0, 10000, 1, 32767)
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
                                  media_links = R7::class_character,
                                  note_links = R7::class_character,
                                  notes = R7::class_character,
                                  certainty = R7::class_character,
                                  
                                  as_ged = R7::new_property(
                                    R7::class_character,
                                    getter = function(self){
                                      
                                      rec_date <- date_to_val(self@recording_date)
                                      
                                      c(
                                        sprintf("0 SOUR %s", self@xref),
                                        sprintf("1 PAGE %s", self@where),
                                        sprintf("1 EVEN %s", self@event_type),
                                        sprintf("2 ROLE %s", rep(self@event_role, length(self@event_type))),
                                        rep("1 DATA", length(rec_date) || length(self@source_text) > 0),
                                        sprintf("2 DATE %s", rec_date),
                                        sprintf("2 TEXT %s", self@source_text),
                                        sprintf("1 OBJE %s", self@media_links),
                                        sprintf("1 NOTE %s", self@note_links),
                                        sprintf("1 NOTE %s", self@notes),
                                        sprintf("1 QUAY %s", self@certainty)
                                      ) 
                                      
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
                                    chk_input_size(self@recording_date, "@recording_date", 0, 1, 1, 35),
                                    chk_input_pattern(self@recording_date, "@recording_date", reg_date_value()),
                                    chk_input_size(self@source_text, "@source_text", 0, 10000, 1, 32767),
                                    chk_input_size(self@media_links, "@media_links", 0, 10000, 3, 22),
                                    chk_input_pattern(self@media_links, "@media_links", reg_xref(TRUE)),
                                    chk_input_size(self@note_links, "@note_links", 0, 10000, 3, 22),
                                    chk_input_pattern(self@note_links, "@note_links", reg_xref(TRUE)),
                                    chk_input_size(self@notes, "@notes", 0, 10000, 1, 32767),
                                    chk_input_size(self@certainty, "@certainty", 0, 1),
                                    chk_input_choice(self@certainty, "@certainty", as.character(0:3))
                                  )
                                }
                                
)



#' @include utils_at.R cls_dates.R cls_validators.R
NULL

class_identifier <- S7::new_class("class_identifier",
                                  properties = list(
                                    user_ids = S7::class_character,
                                    unique_ids = S7::class_character,
                                    external_ids = S7::class_character,
                                    
                                    as_ged = S7::new_property(
                                      S7::class_character,
                                      getter = function(self){
                                        tmp <- character()
                                        for(i in seq_along(self@user_ids)){
                                          tmp <- c(
                                            tmp,
                                            sprintf("0 REFN %s", self@user_ids[i]),
                                            sprintf("1 TYPE %s", names(self@user_ids)[i])
                                          )
                                        }
                                        tmp <- c(
                                          tmp,
                                          sprintf("0 UID %s", self@unique_ids),
                                        )
                                        for(i in seq_along(self@external_ids)){
                                          tmp <- c(
                                            tmp,
                                            sprintf("0 EXID %s", self@external_ids[i]),
                                            sprintf("1 TYPE %s", names(self@external_ids)[i])
                                          )
                                        }
                                        tmp <- tmp[tmp != "1 TYPE "]
                                        tmp
                                      }
                                  ))
                                  )

#' @export
class_media_link <- S7::new_class("media_link",
                                  properties = list(
                                    xref = S7::class_character,
                                    title = S7::class_character,
                                    top = S7::class_integer,
                                    left = S7::class_integer,
                                    height = S7::class_integer,
                                    width = S7::class_integer,
                                    
                                    as_ged = S7::new_property(
                                      S7::class_character,
                                      getter = function(self){
                                        c(
                                          sprintf("0 OBJE %s", self@xref),
                                          "1 CROP",
                                          sprintf("2 TOP %s", self@top),
                                          sprintf("2 LEFT %s", self@left),
                                          sprintf("2 HEIGHT %s", self@height),
                                          sprintf("2 WIDTH %s", self@width),
                                          sprintf("1 TITL %s", self@title)
                                        )
                                      })
                                  ),
                                  validator = function(self) {
                                    c(
                                      chk_input_size(self@xref, "@xref", 1, 1),
                                      chk_input_pattern(self@xref, "@xref", reg_xref(TRUE)),
                                      chk_input_size(self@title, "@title", 0, 1, 1),
                                      chk_input_size(self@top, "@top", 0, 1, 1),
                                      chk_input_size(self@left, "@left", 0, 1, 1),
                                      chk_input_size(self@height, "@height", 0, 1, 1),
                                      chk_input_size(self@width, "@width", 0, 1, 1)
                                    )
                                  }
)

#' @export
class_note <- S7::new_class("class_note",
                            properties = list(
                              text = S7::class_character,
                              language = S7::class_character,
                              media_type = S7::class_character,
                              translations = class_list,
                              citations = S7::class_list,
                              
                              as_ged = S7::new_property(
                                S7::class_character,
                                getter = function(self){
                                  c(
                                    sprintf("0 NOTE %s", self@text),
                                    sprintf("1 MIME %s", self@media_type),
                                    sprintf("1 LANG %s", self@language),
                                    lst_to_ged(self@translations) |> increase_level(by = 1),
                                    lst_to_ged(self@citations) |> increase_level(by = 1),
                                  )
                                })
                            ),
                            validator = function(self){
                              c(
                                chk_input_size(self@text, "@text", 1, 1, 1),
                                chk_input_size(self@language, "@language", 0, 1),
                                #TODO: language option
                                chk_input_size(self@media_type, "@media_type", 0, 1),
                                #TODO: media type pattern
                                chk_input_S7classes(self@translations, "@translations", class_translation_note),
                                chk_input_S7classes(self@citations, "@citations", class_citation)
                              )
                            }
                            )

#' @export
class_change_date <- S7::new_class("class_change_date",
                                   properties = list(
                                     date = S7::new_property(S7::new_union(class_date_exact, S7::class_character), 
                                                             default = date_exact_current()),
                                     time = S7::class_character,
                                     note_links = S7::class_character,
                                     notes = S7::new_property(S7::new_union(S7::class_character, S7::class_list)),
                                     
                                     as_ged = S7::new_property(
                                       S7::class_character,
                                       getter = function(self){
                                         c(
                                           "0 CHAN",
                                           sprintf("1 DATE %s", date_to_val(self@date)),
                                           sprintf("2 TIME %s", self@time),
                                           sprintf("1 SNOTE %s", self@note_links),
                                           lst_to_ged(self@notes) |> increase_level(by = 1)
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



#' @export
class_citation <- S7::new_class("class_citation",
                                properties = list(
                                  xref = S7::class_character,
                                  where = S7::class_character,
                                  event_type = S7::class_character,
                                  event_role = S7::class_character,
                                  recording_date = S7::new_property(S7::new_union(NULL, 
                                                                                  class_date_calendar,
                                                                                  class_date_period,
                                                                                  class_date_range,
                                                                                  class_date_approx, 
                                                                                  S7::class_character)),
                                  source_text = S7::class_character,
                                  media_links = S7::class_character,
                                  note_links = S7::class_character,
                                  notes = S7::class_character,
                                  certainty = S7::class_character,
                                  
                                  as_ged = S7::new_property(
                                    S7::class_character,
                                    getter = function(self){
                                      
                                      rec_date <- date_to_val(self@recording_date)
                                      
                                      c(
                                        sprintf("0 SOUR %s", self@xref),
                                        sprintf("1 PAGE %s", self@where),
                                        sprintf("1 EVEN %s", self@event_type),
                                        sprintf("2 ROLE %s", rep(self@event_role, length(self@event_type))),
                                        rep("1 DATA", length(rec_date) + length(self@source_text) > 0),
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



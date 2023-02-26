#' @include cls_validators.R 
NULL

#' @export
#' @include cls_record_support.R
class_note <- S7::new_class("class_note",
                            properties = list(
                              text = S7::class_character,
                              language = S7::class_character,
                              media_type = S7::class_character,
                              alt_text = S7::class_list,
                              #citations = S7::class_list,
                              
                              as_ged = S7::new_property(
                                S7::class_character,
                                getter = function(self){
                                  c(
                                    sprintf("0 NOTE %s", self@text),
                                    sprintf("1 MIME %s", self@media_type),
                                    sprintf("1 LANG %s", self@language),
                                    lst_to_ged(self@alt_text) |> increase_level(by = 1)
                                    #   lst_to_ged(self@citations) |> increase_level(by = 1),
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
                                chk_input_S7classes(self@alt_text, "@alt_text", class_translation_txt)
                                #  chk_input_S7classes(self@citations, "@citations", class_citation)
                              )
                            }
)

#' @export
class_media_link <- S7::new_class("class_media_link",
                                  properties = list(
                                    media_uid = S7::class_character,
                                    title = S7::class_character,
                                    crop = S7::new_property(S7::class_logical, default = FALSE),
                                    top = S7::new_property(S7::class_numeric, default = 0),
                                    left = S7::new_property(S7::class_numeric, default = 0),
                                    height = S7::class_numeric,
                                    width = S7::class_numeric,
                                    
                                    as_ged = S7::new_property(
                                      S7::class_character,
                                      getter = function(self){
                                        c(
                                          sprintf("0 OBJE %s", self@media_uid),
                                          rep("1 CROP", self@crop),
                                          rep(sprintf("2 TOP %s", self@top), self@crop),
                                          rep(sprintf("2 LEFT %s", self@left), self@crop),
                                          rep(sprintf("2 HEIGHT %s", self@height), self@crop),
                                          rep(sprintf("2 WIDTH %s", self@width), self@crop),
                                          sprintf("1 TITL %s", self@title)
                                        )
                                      })
                                  ),
                                  validator = function(self) {
                                    tt_err <- ll_err <- hh_err <- ww_err <- NULL
                                    if(length(self@top) == 1 && floor(self@top) != self@top)
                                      tt_err <- "Top must be a whole number"
                                    if(length(self@left) == 1 && floor(self@left) != self@left)
                                      ll_err <- "Left must be a whole number"
                                    if(length(self@height) == 1 && floor(self@height) != self@height)
                                      hh_err <- "Height must be a whole number"
                                    if(length(self@width) == 1 && floor(self@width) != self@width)
                                      ww_err <- "Width must be a whole number"
                                    c(
                                      tt_err, ll_err, hh_err, ww_err,
                                      chk_input_size(self@media_uid, "@media_uid", 1, 1),
                                      chk_input_pattern(self@media_uid, "@media_uid", reg_uuid(TRUE)),
                                      chk_input_size(self@title, "@title", 0, 1, 1),
                                      chk_input_size(self@top, "@top", 0, 1, 0),
                                      chk_input_size(self@left, "@left", 0, 1, 0),
                                      chk_input_size(self@height, "@height", 0, 1, 1),
                                      chk_input_size(self@width, "@width", 0, 1, 1)
                                    )
                                  }
)


#' @export
#' @include cls_dates.R cls_record_support.R
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
                                  recording_time = S7::new_property(S7::new_union(NULL, class_time, S7::class_character)),
                                  source_text = S7::class_character,
                                  media_links = S7::class_character,
                                  note_links = S7::class_character,
                                  notes = S7::new_property(S7::new_union(S7::class_character, S7::class_list)),
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



#' @export
#' @include cls_dates.R
class_creation_date <- S7::new_class("class_creation_date",
                                     properties = list(
                                       date = S7::new_property(S7::new_union(NULL, class_date_exact, S7::class_character), 
                                                               default = date_exact_current()),
                                       time = S7::new_property(S7::new_union(NULL, class_time, S7::class_character)),
                                       
                                       as_ged = S7::new_property(
                                         S7::class_character,
                                         getter = function(self){
                                           c(
                                             "0 CREA",
                                             sprintf("1 DATE %s", date_to_val(self@date)),
                                             sprintf("2 TIME %s", self@time)
                                           )
                                         })
                                     ),
                                     validator = function(self) {
                                       c(
                                         chk_input_size(self@date, "@date", 1, 1),
                                         chk_input_pattern(self@date, "@date", reg_date_exact()),
                                         chk_input_size(self@time, "@time", 0, 1),
                                         chk_input_pattern(self@time, "@time", reg_time())
                                       )
                                     }
)

#' @export
class_change_date <- S7::new_class("class_change_date", parent = class_creation_date,
                                   properties = list(
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
                                       chk_input_pattern(self@note_links, "@note_links", reg_xref(TRUE)),
                                       chk_input_size(self@notes, "@notes", min_char = 1)
                                     )
                                   }
)


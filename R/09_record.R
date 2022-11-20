
class_record <- R7::new_class("class_record", # TODO: abstract = TRUE,
                              properties = list(
                                xref = R7::new_property(R7::class_character),
                                change_date = R7::new_property(R7::new_union(NULL, class_change_date))
                              ),
                              validator = function(self){
                                c(
                                  chk_input_size(self@xref, "@xref", 1, 1, 3, 22),
                                  chk_input_pattern(self@xref, "@xref", reg_xref(TRUE)),
                                  chk_input_size(self@change_date, "@change_date", 0, 1)
                                )
                              }
)


class_record_subm <- R7::new_class("class_record_subm", parent = class_record,
                                   properties = list(
                                     name = R7::class_character,
                                     address = R7::new_property(R7::new_union(NULL, class_address)),
                                     media_links = R7::class_list,
                                     notes = R7::class_list,
                                     
                                     as_df = R7::new_property(R7::class_data.frame,
                                                              getter = function(self){
                                                                
                                                                dplyr::bind_rows(
                                                                  df_rows(level = 0, record = self@xref, tag = "SUBM", value = ""),
                                                                  df_rows(level = 1, tag = "NAME", value = self@name),
                                                                  obj_to_df(self@address, level_inc = 1),
                                                                  lst_to_df(self@media_links, level_inc = 1),
                                                                  lst_to_df(self@notes, level_inc = 1),
                                                                  obj_to_df(self@change_date, level_inc = 1)
                                                                ) |> tidyr::fill(record)
                                                                
                                                              })
                                   )
)

class_record_lin <- R7::new_class("class_record_lin", parent = class_record, #TODO: abstract = TRUE,
                              properties = list(
                                user_reference_numbers = R7::class_character,
                                
                                refs_df = R7::new_property(R7::class_data.frame,
                                                           getter = function(self){
                                                             tmp <- tibble::tibble()
                                                             for(i in seq_along(self@user_reference_numbers)){
                                                               tmp <- dplyr::bind_rows(
                                                                 tmp,
                                                                 df_rows(level = 1, tag = "REFN", value = self@user_reference_numbers[i]),
                                                                 df_rows(level = 2, tag = "TYPE", value = names(self@user_reference_numbers)[i])
                                                               ) |>
                                                                 dplyr::filter(!(tag == "TYPE" & value == ""))
                                                             }
                                                             tmp
                                                           })
                              ),
                              validator = function(self){
                                c(
                                  chk_input_size(self@user_reference_numbers, "@user_reference_numbers", 0, 10000, 1, 20),
                                  chk_input_size(names(self@user_reference_numbers), "@user_reference_numbers types", 0, 10000, 0, 40)
                                )
                              }
)

class_record_famg <- R7::new_class("class_record_famg", parent = class_record_lin,
                               properties = list(
                                 events = R7::class_list,
                                 husb_xref = R7::class_character,
                                 wife_xref = R7::class_character,
                                 chil_xref = R7::class_character,
                                 num_children = R7::class_integer,
                                 notes = R7::class_list,
                                 citations = R7::class_list,
                                 media_links = R7::class_list
                               ))

class_record_indi <- R7::new_class("class_record_indi", parent = class_record_lin,
                               properties = list(
                                 personal_names = R7::class_list,
                                 sex = R7::new_property(R7::class_character, default = "U"),
                                 facts = R7::class_list,
                                 family_links = R7::class_list,
                                 associations = R7::class_list,
                                 notes = R7::class_list,
                                 citations = R7::class_list,
                                 media_links = R7::class_list
                               ))

class_record_media <- R7::new_class("class_record_media", parent = class_record_lin,
                                properties = list(
                                  file_ref = R7::class_character,
                                  format = R7::class_character,
                                  media_type = R7::class_character,
                                  title = R7::class_character,
                                  notes = R7::class_list,
                                  citations = R7::class_list,
                                  
                                  as_df = R7::new_property(R7::class_data.frame,
                                                           getter = function(self){
                                                             dplyr::bind_rows(
                                                               df_rows(level = 0, record = self@xref, tag = "OBJE", value = ""),
                                                               df_rows(level = 1, tag = "FILE", value = self@file_ref),
                                                               df_rows(level = 2, tag = "FORM", value = self@format),
                                                               df_rows(level = 3, tag = "TYPE", value = rep(self@media_type, length(self@format))),
                                                               df_rows(level = 2, tag = "TITL", value = rep(self@title, length(self@file_ref))),
                                                               self@refs_df,
                                                               lst_to_df(self@notes, level_inc = 1),
                                                               lst_to_df(self@citations, level_inc = 1),
                                                               obj_to_df(self@change_date, level_inc = 1)
                                                             ) |> tidyr::fill(record)
                                                           })
                                ),
                                validator = function(self){
                                  c(
                                    chk_input_size(self@file_ref, "@file_ref", 1, 1, 1, 259),
                                    chk_input_size(self@format, "@format", 1, 1),
                                    chk_input_choice(self@format, "@format", val_multimedia_formats()),
                                    chk_input_size(self@media_type, "@media_type", 0, 1),
                                    chk_input_choice(self@media_type, "@media_type", val_source_media_types()),
                                    chk_input_size(self@title, "@title", 0, 1, 1, 248),
                                    chk_input_R7classes(self@notes, "@notes", class_note),
                                    chk_input_R7classes(self@citations, "@citations", class_citation)
                                  )
                                }
                                )

class_record_sour <- R7::new_class("class_record_sour", parent = class_record_lin,
                               properties = list(
                                 events_recorded = R7::class_character,
                                 date_period = R7::class_character,
                                 jurisdiction_place = R7::class_character,
                                 responsible_agency = R7::class_character,
                                 data_notes = R7::class_list,
                                 originator = R7::class_character,
                                 full_title = R7::class_character,
                                 short_title = R7::class_character,
                                 publication_facts = R7::class_character,
                                 source_text = R7::class_character,
                                 repo_citations = R7::class_list,
                                 notes = R7::class_list,
                                 media_links = R7::class_list,
                                 
                                 as_df = as_df = R7::new_property(R7::class_data.frame,
                                                                  getter = function(self){
                                                                    sour_df <- dplyr::bind_rows(
                                                                      df_rows(level = 0, record = self@xref, tag = "SOUR", value = ""),
                                                                      df_rows(level = 1, tag = "DATA", value = ""),
                                                                      df_rows(level = 2, tag = "EVEN", value = self@events_recorded),
                                                                      df_rows(level = 3, tag = "DATE", value = self@date_period),
                                                                      df_rows(level = 3, tag = "PLAC", value = self@jurisdiction_place),
                                                                      df_rows(level = 2, tag = "AGNC", value = self@responsible_agency),
                                                                      lst_to_df(self@data_notes, level_inc = 2),
                                                                      df_rows(level = 1, tag = "AUTH", value = self@originator),
                                                                      df_rows(level = 1, tag = "TITL", value = self@full_title),
                                                                      df_rows(level = 1, tag = "ABBR", value = self@short_title),
                                                                      df_rows(level = 1, tag = "PUBL", value = self@publication_facts),
                                                                      df_rows(level = 1, tag = "TEXT", value = self@source_text),
                                                                      lst_to_df(self@repo_citations, level_inc = 1),
                                                                      self@refs_df,
                                                                      obj_to_df(self@change_date, level_inc = 1),
                                                                      lst_to_df(self@notes, level_inc = 1),
                                                                      lst_to_df(self@media_links, level_inc = 1),
                                                                    ) |> tidyr::fill(record)
                                                                    
                                                                    if (length(self@date_period) + length(self@jurisdiction_place) == 0)
                                                                      sour_df <- dplyr::filter(sour_df, tag != "EVEN")
                                                                    
                                                                    if (length(self@events_recorded) + length(self@responsible_agency) + length(self@data_notes) == 0)
                                                                      sour_df <- dplyr::filter(sour_df, tag != "DATA")
                                                                    
                                                                    sour_df
                                                                  })
                               ),
                               validator = function(self){
                                 c(
                                   
                                 )
                               })

class_record_repo <- R7::new_class("class_record_repo", parent = class_record_lin,
                               properties = list(
                                 name = R7::class_character,
                                 address = R7::new_property(R7::new_union(NULL, class_address)),
                                 notes = R7::class_list,
                                 
                                 as_df = R7::new_property(R7::class_data.frame,
                                                          getter = function(self){
                                                            dplyr::bind_rows(
                                                              df_rows(level = 0, record = self@xref, tag = "REPO", value = ""),
                                                              df_rows(level = 1, tag = "NAME", value = self@name),
                                                              obj_to_df(self@address, level_inc = 1),
                                                              lst_to_df(self@notes, level_inc = 1),
                                                              self@refs_df,
                                                              obj_to_df(self@change_date, level_inc = 1)
                                                            ) |> tidyr::fill(record)
                                                          })
                               ),
                               validator = function(self){
                                 c(
                                   chk_input_size(self@name, "@name", 1, 1, 1, 90),
                                   chk_input_size(self@address, "@address", 0, 1),
                                   chk_input_R7classes(self@notes, "@notes", class_note)
                                 )
                               }
                               )

class_record_note <- R7::new_class("class_record_note", parent = class_record_lin,
                                   properties = list(
                                     text = R7::class_character,
                                     citations = R7::class_list,
                                     
                                     as_df = R7::new_property(R7::class_data.frame,
                                                              getter = function(self){
                                                                dplyr::bind_rows(
                                                                  df_rows(level = 0, record = self@xref, tag = "NOTE", value = self@text),
                                                                  self@refs_df,
                                                                  lst_to_df(self@citations, level_inc = 1),
                                                                  obj_to_df(self@change_date, level_inc = 1)
                                                                ) |> tidyr::fill(record)
                                                              })
                                   ),
                                   validator = function(self){
                                     c(
                                       chk_input_size(self@text, "@text", 1, 1, 1, 32767),
                                       chk_input_R7classes(self@citations, "@citations", class_citation)
                                     )
                                   }
)



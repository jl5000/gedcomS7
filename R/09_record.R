
class_record <- 
  R7::new_class("class_record", #abstract = TRUE,
                properties = list(
                  xref = R7::class_character,
                  user_reference_numbers = R7::class_character,
                  auto_id = R7::class_character,
                  media_links = R7::class_character,
                  note_links = R7::class_character,
                  notes = R7::class_character,
                  citations = R7::class_list,
                  last_updated = R7::new_property(R7::new_union(NULL, class_change_date)),
                  
                  refs_df = R7::new_property(
                    R7::class_data.frame,
                    getter = function(self){
                      tmp <- NULL
                      for(i in seq_along(self@user_reference_numbers)){
                        tmp <- rbind(
                          tmp,
                          df_rows(level = 1, tag = "REFN", value = self@user_reference_numbers[i]),
                          df_rows(level = 2, tag = "TYPE", value = names(self@user_reference_numbers)[i])
                        )[!(tag == "TYPE" & value == "")]
                      }
                      tmp
                    })
                ),
                validator = function(self){
                  c(
                    chk_input_size(self@xref, "@xref", 1, 1, 3, 22),
                    chk_input_pattern(self@xref, "@xref", reg_xref(TRUE)),
                    chk_input_size(self@user_reference_numbers, "@user_reference_numbers", 0, 10000, 1, 20),
                    chk_input_size(names(self@user_reference_numbers), "@user_reference_numbers types", 0, 10000, 0, 40),
                    chk_input_size(self@auto_id, "@auto_id", 0, 1, 1, 12),
                    chk_input_size(self@media_links, "@media_links", 0, 10000, 3, 22),
                    chk_input_pattern(self@media_links, "@media_links", reg_xref(TRUE)),
                    chk_input_size(self@note_links, "@note_links", 0, 10000, 3, 22),
                    chk_input_pattern(self@note_links, "@note_links", reg_xref(TRUE)),
                    chk_input_size(self@notes, "@notes", 0, 10000, 1, 32767),
                    chk_input_R7classes(self@citations, "@citations", class_citation),
                    chk_input_size(self@last_updated, "@last_updated", 0, 1)
                  )
                }
  )


class_record_subm <- 
  R7::new_class("class_record_subm", parent = class_record,
                properties = list(
                  name = R7::new_property(R7::class_character, default = unname(Sys.info()["user"])),
                  address = R7::new_property(R7::new_union(NULL, class_address)),
                  
                  as_df = R7::new_property(
                    R7::class_data.frame,
                    getter = function(self){
                      df <- rbind(
                        df_rows(level = 0, tag = "SUBM", value = ""),
                        df_rows(level = 1, tag = "NAME", value = self@name),
                        obj_to_df(self@address, level_inc = 1),
                        df_rows(level = 1, tag = "OBJE", value = self@media_links),
                        df_rows(level = 1, tag = "RIN", value = self@auto_id),
                        df_rows(level = 1, tag = "NOTE", value = self@note_links),
                        df_rows(level = 1, tag = "NOTE", value = self@notes),
                        obj_to_df(self@last_updated, level_inc = 1)
                      )
                      df[,record:= self@xref]
                      data.table::setcolorder(df, c(4,1,2,3))
                      df[]
                    })
                ),
                validator = function(self){
                  c(
                    chk_input_size(self@name, "@name", 1, 1, 1, 60),
                    chk_input_size(self@address, "@address", 0, 1),
                    chk_input_size(self@user_reference_numbers, "@user_reference_numbers", 0, 0),
                    chk_input_size(self@citations, "@citations", 0, 0)
                  )
                }
  )

class_record_famg <- 
  R7::new_class("class_record_famg", parent = class_record,
                properties = list(
                  events = R7::class_list,
                  husb_xref = R7::class_character,
                  wife_xref = R7::class_character,
                  chil_xref = R7::class_character,
                  num_children = R7::class_integer,
                  
                  as_df = R7::new_property(
                    R7::class_data.frame,
                    getter = function(self){
                      df <- rbind(
                        df_rows(level = 0, tag = "FAM", value = ""),
                        lst_to_df(self@events, level_inc = 1),
                        df_rows(level = 1, tag = "HUSB", value = self@husb_xref),
                        df_rows(level = 1, tag = "WIFE", value = self@wife_xref),
                        df_rows(level = 1, tag = "CHIL", value = self@chil_xref),
                        df_rows(level = 1, tag = "NCHI", value = as.character(self@num_children)),
                        self@refs_df,
                        df_rows(level = 1, tag = "RIN", value = self@auto_id),
                        obj_to_df(self@last_updated, level_inc = 1),
                        df_rows(level = 1, tag = "NOTE", value = self@note_links),
                        df_rows(level = 1, tag = "NOTE", value = self@notes),
                        lst_to_df(self@citations, level_inc = 1),
                        df_rows(level = 1, tag = "OBJE", value = self@media_links)
                      )
                      df[,record:= self@xref]
                      data.table::setcolorder(df, c(4,1,2,3))
                      df[]
                    })
                ),
                validator = function(self){
                  c(
                    chk_input_R7classes(self@events, "@events", class_famg_fact),
                    chk_input_size(self@husb_xref, "@husb_xref", 0, 1, 3, 22),
                    chk_input_pattern(self@husb_xref, "@husb_xref", reg_xref(TRUE)),
                    chk_input_size(self@wife_xref, "@wife_xref", 0, 1, 3, 22),
                    chk_input_pattern(self@wife_xref, "@wife_xref", reg_xref(TRUE)),
                    chk_input_size(self@chil_xref, "@chil_xref", 0, 10000, 3, 22),
                    chk_input_pattern(self@chil_xref, "@chil_xref", reg_xref(TRUE)),
                    chk_input_size(self@num_children, "@num_children", 0, 1, 1, 3)
                  )
                })

class_record_indi <- 
  R7::new_class("class_record_indi", parent = class_record,
                properties = list(
                  personal_names = R7::class_list,
                  sex = R7::new_property(R7::class_character, default = "U"),
                  facts = R7::class_list,
                  family_links = R7::class_list,
                  associations = R7::class_list,
                  
                  primary_name = R7::new_property(
                    R7::class_character,
                    getter = function(self){
                      if(length(self@personal_names) == 0){
                        character()
                      } else {
                        self@personal_names[[1]]@name@full |>
                          gsub(pattern = "/", replacement = "")
                      }
                    }),
                  
                  all_names = R7::new_property(
                    R7::class_character,
                    getter = function(self){
                      sapply(self@personal_names, \(nm){
                        gsub(nm@name@full, pattern = "/", replacement = "")
                      }, USE.NAMES = FALSE)
                    }),
                  
                  desc_short = R7::new_property(
                    R7::class_character,
                    getter = function(self){
                      if(length(self@primary_name) == 0){
                        name <- "Unnamed individual"
                      } else {
                        name <- self@primary_name
                      }
                      paste0("Individual ", self@xref, ", ", name)
                    }),
                  
                  birth_date = R7::new_property(
                    R7::class_character,
                    getter = function(self){
                      for(fact in self@facts){
                        if(R7::R7_inherits(fact, birth)) return(fact@details@event_date)
                      }
                      character()
                    }),
                  
                  birth_place = R7::new_property(
                    R7::class_character,
                    getter = function(self){
                      for(fact in self@facts){
                        if(R7::R7_inherits(fact, birth)) return(fact@details@event_location)
                      }
                      character()
                    }),
                  
                  is_alive = R7::new_property(
                    R7::class_logical,
                    getter = function(self){
                      for(fact in self@facts){
                        if(R7::R7_inherits(fact, death)) return(FALSE)
                      }
                      TRUE
                    }),
                  
                  death_date = R7::new_property(
                    R7::class_character,
                    getter = function(self){
                      for(fact in self@facts){
                        if(R7::R7_inherits(fact, death)) return(fact@details@event_date)
                      }
                      character()
                    }),
                  
                  death_place = R7::new_property(
                    R7::class_character,
                    getter = function(self){
                      for(fact in self@facts){
                        if(R7::R7_inherits(fact, death)) return(fact@details@event_location)
                      }
                      character()
                    }),
                  
                  as_df = R7::new_property(
                    R7::class_data.frame,
                    getter = function(self){
                      df <- rbind(
                        df_rows(level = 0, tag = "INDI", value = ""),
                        lst_to_df(self@personal_names, level_inc = 1),
                        df_rows(level = 1, tag = "SEX", value = self@sex),
                        lst_to_df(self@facts, level_inc = 1),
                        lst_to_df(self@family_links, level_inc = 1),
                        lst_to_df(self@associations, level_inc = 1),
                        self@refs_df,
                        df_rows(level = 1, tag = "RIN", value = self@auto_id),
                        obj_to_df(self@last_updated, level_inc = 1),
                        df_rows(level = 1, tag = "NOTE", value = self@note_links),
                        df_rows(level = 1, tag = "NOTE", value = self@notes),
                        lst_to_df(self@citations, level_inc = 1),
                        df_rows(level = 1, tag = "OBJE", value = self@media_links)
                      )
                      df[,record:= self@xref]
                      data.table::setcolorder(df, c(4,1,2,3))
                      df[]
                    })
                ),
                validator = function(self){
                  c(
                    chk_input_R7classes(self@personal_names, "@personal_names", class_personal_name),
                    chk_input_size(self@sex, "@sex", 0, 1),
                    chk_input_choice(self@sex, "@sex", val_sexes()),
                    chk_input_R7classes(self@facts, "@facts", class_indi_fact),
                    chk_input_R7classes(self@family_links, "@family_links", class_family_link),
                    chk_input_R7classes(self@associations, "@associations", class_association)
                  )
                }
  )

class_record_media <- 
  R7::new_class("class_record_media", parent = class_record,
                properties = list(
                  file_ref = R7::class_character,
                  format = R7::class_character,
                  media_type = R7::class_character,
                  title = R7::class_character,
                  
                  as_df = R7::new_property(
                    R7::class_data.frame,
                    getter = function(self){
                      df <- rbind(
                        df_rows(level = 0, tag = "OBJE", value = ""),
                        df_rows(level = 1, tag = "FILE", value = self@file_ref),
                        df_rows(level = 2, tag = "FORM", value = self@format),
                        df_rows(level = 3, tag = "TYPE", value = rep(self@media_type, length(self@format))),
                        df_rows(level = 2, tag = "TITL", value = rep(self@title, length(self@file_ref))),
                        self@refs_df,
                        df_rows(level = 1, tag = "RIN", value = self@auto_id),
                        df_rows(level = 1, tag = "NOTE", value = self@note_links),
                        df_rows(level = 1, tag = "NOTE", value = self@notes),
                        lst_to_df(self@citations, level_inc = 1),
                        obj_to_df(self@last_updated, level_inc = 1)
                      )
                      df[,record:= self@xref]
                      data.table::setcolorder(df, c(4,1,2,3))
                      df[]
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
                    chk_input_size(self@media_links, "@media_links", 0, 0)
                  )
                }
  )

class_record_sour <- 
  R7::new_class("class_record_sour", parent = class_record,
                properties = list(
                  events_recorded = R7::class_character,
                  date_period = R7::new_property(R7::new_union(NULL, class_date_period, R7::class_character)),
                  jurisdiction_place = R7::class_character,
                  responsible_agency = R7::class_character,
                  data_note_links = R7::class_character,
                  data_notes = R7::class_character,
                  originator = R7::class_character,
                  full_title = R7::class_character,
                  short_title = R7::class_character,
                  publication_facts = R7::class_character,
                  source_text = R7::class_character,
                  repo_citations = R7::class_list,
                  
                  as_df = R7::new_property(
                    R7::class_data.frame,
                    getter = function(self){
                      sour_df <- rbind(
                        df_rows(level = 0, tag = "SOUR", value = ""),
                        df_rows(level = 1, tag = "DATA", value = ""),
                        df_rows(level = 2, tag = "EVEN", value = self@events_recorded),
                        date_to_df(self@date_period, level_inc = 3),
                        df_rows(level = 3, tag = "PLAC", value = self@jurisdiction_place),
                        df_rows(level = 2, tag = "AGNC", value = self@responsible_agency),
                        df_rows(level = 2, tag = "NOTE", value = self@data_note_links),
                        df_rows(level = 2, tag = "NOTE", value = self@data_notes),
                        df_rows(level = 1, tag = "AUTH", value = self@originator),
                        df_rows(level = 1, tag = "TITL", value = self@full_title),
                        df_rows(level = 1, tag = "ABBR", value = self@short_title),
                        df_rows(level = 1, tag = "PUBL", value = self@publication_facts),
                        df_rows(level = 1, tag = "TEXT", value = self@source_text),
                        lst_to_df(self@repo_citations, level_inc = 1),
                        self@refs_df,
                        df_rows(level = 1, tag = "RIN", value = self@auto_id),
                        obj_to_df(self@last_updated, level_inc = 1),
                        df_rows(level = 1, tag = "NOTE", value = self@note_links),
                        df_rows(level = 1, tag = "NOTE", value = self@notes),
                        df_rows(level = 1, tag = "OBJE", value = self@media_links)
                      )
                      sour_df[,record:= self@xref]
                      data.table::setcolorder(sour_df, c(4,1,2,3))
                      
                      if (length(self@date_period) + length(self@jurisdiction_place) == 0)
                        sour_df <- sour_df[tag != "EVEN"]
                      
                      if (length(self@events_recorded) + length(self@responsible_agency) + length(self@data_notes) == 0)
                        sour_df <- sour_df[tag != "DATA"]
                      
                      sour_df
                    })
                ),
                validator = function(self){
                  c(
                    # chk_input_size(self@events_recorded, "@events_recorded", 1, 1, 1, 259), # TODO
                    chk_input_size(self@date_period, "@date_period", 0, 1),
                    chk_input_pattern(self@date_period, "@date_period", reg_date_period()),
                    # chk_input_size(self@jurisdiction_place, "@jurisdiction_place", 1, 1, 1, 259), # TODO
                    chk_input_size(self@responsible_agency, "@responsible_agency", 0, 1, 1, 120),
                    chk_input_size(self@data_note_links, "@data_note_links", 0, 10000, 3, 22),
                    chk_input_pattern(self@data_note_links, "@data_note_links", reg_xref(TRUE)),
                    chk_input_size(self@data_notes, "@data_notes", 0, 10000, 1, 32767),
                    chk_input_size(self@originator, "@originator", 0, 1, 1, 255),
                    chk_input_size(self@full_title, "@full_title", 0, 1, 1, 4095),
                    chk_input_size(self@short_title, "@short_title", 0, 1, 1, 60),
                    chk_input_size(self@publication_facts, "@publication_facts", 0, 1, 1, 4095),
                    chk_input_size(self@source_text, "@source_text", 0, 1, 1, 32767),
                    chk_input_R7classes(self@repo_citations, "@repo_citations", class_repository_citation),
                    chk_input_size(self@citations, "@citations", 0, 0)
                  )
                })

class_record_repo <- 
  R7::new_class("class_record_repo", parent = class_record,
                properties = list(
                  name = R7::class_character,
                  address = R7::new_property(R7::new_union(NULL, class_address)),
                  
                  as_df = R7::new_property(
                    R7::class_data.frame,
                    getter = function(self){
                      df <- rbind(
                        df_rows(level = 0, tag = "REPO", value = ""),
                        df_rows(level = 1, tag = "NAME", value = self@name),
                        obj_to_df(self@address, level_inc = 1),
                        df_rows(level = 1, tag = "NOTE", value = self@note_links),
                        df_rows(level = 1, tag = "NOTE", value = self@notes),
                        self@refs_df,
                        df_rows(level = 1, tag = "RIN", value = self@auto_id),
                        obj_to_df(self@last_updated, level_inc = 1)
                      )
                      df[,record:= self@xref]
                      data.table::setcolorder(df, c(4,1,2,3))
                      df[]
                    })
                ),
                validator = function(self){
                  c(
                    chk_input_size(self@name, "@name", 1, 1, 1, 90),
                    chk_input_size(self@address, "@address", 0, 1),
                    chk_input_size(self@citations, "@citations", 0, 0),
                    chk_input_size(self@media_links, "@media_links", 0, 0)
                  )
                }
  )

class_record_note <- 
  R7::new_class("class_record_note", parent = class_record,
                properties = list(
                  text = R7::class_character,
                  
                  as_df = R7::new_property(
                    R7::class_data.frame,
                    getter = function(self){
                      df <- rbind(
                        df_rows(level = 0, tag = "NOTE", value = self@text),
                        self@refs_df,
                        df_rows(level = 1, tag = "RIN", value = self@auto_id),
                        lst_to_df(self@citations, level_inc = 1),
                        obj_to_df(self@last_updated, level_inc = 1)
                      )
                      df[,record:= self@xref]
                      data.table::setcolorder(df, c(4,1,2,3))
                      df[]
                    })
                ),
                validator = function(self){
                  c(
                    chk_input_size(self@text, "@text", 1, 1, 1, 32767),
                    chk_input_size(self@notes, "@notes", 0, 0),
                    chk_input_size(self@note_links, "@note_links", 0, 0),
                    chk_input_size(self@media_links, "@media_links", 0, 0)
                  )
                }
  )

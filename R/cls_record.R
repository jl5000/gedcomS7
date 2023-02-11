#' @include utils_at.R cls_common.R cls_locations.R cls_facts.R cls_validators.R
NULL

class_record <- 
  R7::new_class("class_record", #abstract = TRUE,
                properties = list(
                  xref = R7::new_property(R7::class_character, default = "@gedcomR7orphan@"),
                  user_reference_numbers = R7::class_character,
                  auto_id = R7::class_character,
                  media_links = R7::class_character,
                  note_links = R7::class_character,
                  notes = R7::class_character,
                  citations = R7::class_list,
                  last_updated = R7::new_property(R7::new_union(NULL, class_change_date)),
                  
                  refs_ged = R7::new_property(
                    R7::class_character,
                    getter = function(self){
                      tmp <- character()
                      for(i in seq_along(self@user_reference_numbers)){
                        tmp <- c(
                          tmp,
                          sprintf("1 REFN %s", self@user_reference_numbers[i]),
                          sprintf("2 TYPE %s", names(self@user_reference_numbers)[i])
                        )
                      }
                      tmp <- tmp[tmp != "2 TYPE "]
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


class_subm <- 
  R7::new_class("class_subm", parent = class_record,
                properties = list(
                  name = R7::new_property(R7::class_character, default = unname(Sys.info()["user"])),
                  address = R7::new_property(R7::new_union(NULL, class_address)),
                  
                  as_ged = R7::new_property(
                    R7::class_character,
                    getter = function(self){
                      c(
                        sprintf("0 %s SUBM", self@xref),
                        sprintf("1 NAME %s", self@name),
                        obj_to_ged(self@address) |> increase_level(by = 1),
                        sprintf("1 OBJE %s", self@media_links),
                        sprintf("1 RIN %s", self@auto_id),
                        sprintf("1 NOTE %s", self@note_links),
                        sprintf("1 NOTE %s", self@notes),
                        obj_to_ged(self@last_updated) |> increase_level(by = 1)
                      )
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

#' @export
class_record_famg <- 
  R7::new_class("class_record_famg", parent = class_record,
                properties = list(
                  facts = R7::class_list,
                  husb_xref = R7::class_character,
                  wife_xref = R7::class_character,
                  chil_biol_xref = R7::class_character,
                  chil_adop_xref = R7::class_character,
                  chil_fost_xref = R7::class_character,
                  num_children = R7::class_integer,
                  
                  relationship_date = R7::new_property(
                    R7::class_character,
                    getter = function(self){
                      for(fact in self@facts){
                        if(fact@fact == "MARR") return(fact@fact_date)
                      }
                      character()
                    }),
                  
                  relationship_place = R7::new_property(
                    R7::class_character,
                    getter = function(self){
                      for(fact in self@facts){
                        if(fact@fact == "MARR") return(fact@fact_location)
                      }
                      character()
                    }),
                  
                  as_ged = R7::new_property(
                    R7::class_character,
                    getter = function(self){
                      c(
                        sprintf("0 %s FAM", self@xref),
                        lst_to_ged(self@facts) |> increase_level(by = 1),
                        sprintf("1 HUSB %s", self@husb_xref),
                        sprintf("1 WIFE %s", self@wife_xref),
                        sprintf("1 CHIL %s", c(self@chil_biol_xref,
                                               self@chil_adop_xref,
                                               self@chil_fost_xref)),
                        sprintf("1 NCHI %s", self@num_children),
                        self@refs_ged,
                        sprintf("1 RIN %s", self@auto_id),
                        obj_to_ged(self@last_updated) |> increase_level(by = 1),
                        sprintf("1 NOTE %s", self@note_links),
                        sprintf("1 NOTE %s", self@notes),
                        lst_to_ged(self@citations) |> increase_level(by = 1),
                        sprintf("1 OBJE %s", self@media_links)
                      )
                    })
                ),
                validator = function(self){
                  c(
                    chk_input_R7classes(self@facts, "@facts", class_fact_famg),
                    chk_input_size(self@husb_xref, "@husb_xref", 0, 1, 3, 22),
                    chk_input_pattern(self@husb_xref, "@husb_xref", reg_xref(TRUE)),
                    chk_input_size(self@wife_xref, "@wife_xref", 0, 1, 3, 22),
                    chk_input_pattern(self@wife_xref, "@wife_xref", reg_xref(TRUE)),
                    chk_input_size(self@chil_biol_xref, "@chil_biol_xref", 0, 10000, 3, 22),
                    chk_input_pattern(self@chil_biol_xref, "@chil_biol_xref", reg_xref(TRUE)),
                    chk_input_size(self@chil_adop_xref, "@chil_adop_xref", 0, 10000, 3, 22),
                    chk_input_pattern(self@chil_adop_xref, "@chil_adop_xref", reg_xref(TRUE)),
                    chk_input_size(self@chil_fost_xref, "@chil_fost_xref", 0, 10000, 3, 22),
                    chk_input_pattern(self@chil_fost_xref, "@chil_fost_xref", reg_xref(TRUE)),
                    chk_input_size(self@num_children, "@num_children", 0, 1, 1, 3)
                  )
                })

#' @export
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
                        if(fact@fact == "BIRT") return(fact@fact_date)
                      }
                      character()
                    }),
                  
                  birth_place = R7::new_property(
                    R7::class_character,
                    getter = function(self){
                      for(fact in self@facts){
                        if(fact@fact == "BIRT") return(fact@fact_location)
                      }
                      character()
                    }),
                  
                  is_alive = R7::new_property(
                    R7::class_logical,
                    getter = function(self){
                      for(fact in self@facts){
                        if(fact@fact == "DEAT") return(FALSE)
                      }
                      TRUE
                    }),
                  
                  death_date = R7::new_property(
                    R7::class_character,
                    getter = function(self){
                      for(fact in self@facts){
                        if(fact@fact == "DEAT") return(fact@fact_date)
                      }
                      character()
                    }),
                  
                  death_place = R7::new_property(
                    R7::class_character,
                    getter = function(self){
                      for(fact in self@facts){
                        if(fact@fact == "DEAT") return(fact@fact_location)
                      }
                      character()
                    }),
                  
                  as_ged = R7::new_property(
                    R7::class_character,
                    getter = function(self){
                      c(
                        sprintf("0 %s INDI", self@xref),
                        lst_to_ged(self@personal_names) |> increase_level(by = 1),
                        sprintf("1 SEX %s", self@sex),
                        lst_to_ged(self@facts) |> increase_level(by = 1),
                        lst_to_ged(self@family_links) |> increase_level(by = 1),
                        lst_to_ged(self@associations) |> increase_level(by = 1),
                        self@refs_ged,
                        sprintf("1 RIN %s", self@auto_id),
                        obj_to_ged(self@last_updated) |> increase_level(by = 1),
                        sprintf("1 NOTE %s", self@note_links),
                        sprintf("1 NOTE %s", self@notes),
                        lst_to_ged(self@citations) |> increase_level(by = 1),
                        sprintf("1 OBJE %s", self@media_links)
                      )
                    })
                ),
                validator = function(self){
                  c(
                    chk_input_R7classes(self@personal_names, "@personal_names", class_personal_name),
                    chk_input_size(self@sex, "@sex", 0, 1),
                    chk_input_choice(self@sex, "@sex", val_sexes()),
                    chk_input_R7classes(self@facts, "@facts", class_fact_indi),
                    chk_input_R7classes(self@family_links, "@family_links", class_spouse_family_link),
                    chk_input_R7classes(self@associations, "@associations", class_association)
                  )
                }
  )

#' @export
class_record_media <- 
  R7::new_class("class_record_media", parent = class_record,
                properties = list(
                  file_ref = R7::class_character,
                  format = R7::class_character,
                  media_type = R7::class_character,
                  title = R7::class_character,
                  
                  as_ged = R7::new_property(
                    R7::class_character,
                    getter = function(self){
                      c(
                        sprintf("0 %s OBJE", self@xref),
                        sprintf("1 FILE %s", self@file_ref),
                        sprintf("2 FORM %s", self@format),
                        sprintf("3 TYPE %s", self@media_type),
                        sprintf("2 TITL %s", self@title),
                        self@refs_ged,
                        sprintf("1 RIN %s", self@auto_id),
                        sprintf("1 NOTE %s", self@note_links),
                        sprintf("1 NOTE %s", self@notes),
                        lst_to_ged(self@citations) |> increase_level(by = 1),
                        obj_to_ged(self@last_updated) |> increase_level(by = 1)
                      )
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

#' @export
class_events_recorded <- 
  R7::new_class("class_events_recorded",
                properties = list(
                  events = R7::class_character,
                  date_period = R7::new_property(R7::new_union(NULL, class_date_period, R7::class_character)),
                  jurisdiction_place = R7::class_character,
                  
                  as_ged = R7::new_property(
                    R7::class_character,
                    getter = function(self){
                      c(
                        sprintf("0 EVEN %s", self@events),
                        sprintf("1 DATE %s", date_to_val(self@date_period)),
                        sprintf("1 PLAC %s", self@jurisdiction_place)
                      )
                    })
                ),
                validator = function(self){
                  c(
                    chk_input_size(self@events, "@events", 1, 1, 1, 90),
                    chk_input_size(self@date_period, "@date_period", 0, 1),
                    chk_input_pattern(self@date_period, "@date_period", reg_date_period()),
                    chk_input_size(self@jurisdiction_place, "@jurisdiction_place", 0, 1, 1, 120)
                  )
                })

#' @export
class_record_sour <- 
  R7::new_class("class_record_sour", parent = class_record,
                properties = list(
                  events_recorded = R7::class_list,
                  responsible_agency = R7::class_character,
                  data_note_links = R7::class_character,
                  data_notes = R7::class_character,
                  originator = R7::class_character,
                  full_title = R7::class_character,
                  short_title = R7::class_character,
                  publication_facts = R7::class_character,
                  source_text = R7::class_character,
                  repo_citations = R7::class_list,
                  
                  as_ged = R7::new_property(
                    R7::class_character,
                    getter = function(self){
                      sour <- c(
                        sprintf("0 %s SOUR", self@xref),
                        "1 DATA",
                        lst_to_ged(self@events_recorded) |> increase_level(by = 2),
                        sprintf("2 AGNC %s", self@responsible_agency),
                        sprintf("2 NOTE %s", self@data_note_links),
                        sprintf("2 NOTE %s", self@data_notes),
                        sprintf("1 AUTH %s", self@originator),
                        sprintf("1 TITL %s", self@full_title),
                        sprintf("1 ABBR %s", self@short_title),
                        sprintf("1 PUBL %s", self@publication_facts),
                        sprintf("1 TEXT %s", self@source_text),
                        lst_to_ged(self@repo_citations) |> increase_level(by = 1),
                        self@refs_ged,
                        sprintf("1 RIN %s", self@auto_id),
                        obj_to_ged(self@last_updated) |> increase_level(by = 1),
                        sprintf("1 NOTE %s", self@note_links),
                        sprintf("1 NOTE %s", self@notes),
                        sprintf("1 OBJE %s", self@media_links)
                      )
                      
                      if (length(self@events_recorded) + length(self@responsible_agency) + 
                          length(self@data_notes) + length(self@data_note_links) == 0)
                        sour <- sour[sour != "1 DATA"]
                      
                      sour
                    })
                ),
                validator = function(self){
                  c(
                    chk_input_R7classes(self@events_recorded, "@events_recorded", class_events_recorded),
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

#' @export
class_record_repo <- 
  R7::new_class("class_record_repo", parent = class_record,
                properties = list(
                  name = R7::class_character,
                  address = R7::new_property(R7::new_union(NULL, class_address)),
                  
                  as_ged = R7::new_property(
                    R7::class_character,
                    getter = function(self){
                      c(
                        sprintf("0 %s REPO", self@xref),
                        sprintf("1 NAME %s", self@name),
                        obj_to_ged(self@address) |> increase_level(by = 1),
                        sprintf("1 NOTE %s", self@note_links),
                        sprintf("1 NOTE %s", self@notes),
                        self@refs_ged,
                        sprintf("1 RIN %s", self@auto_id),
                        obj_to_ged(self@last_updated) |> increase_level(by = 1)
                      )
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

#' @export
class_record_note <- 
  R7::new_class("class_record_note", parent = class_record,
                properties = list(
                  text = R7::class_character,
                  
                  as_ged = R7::new_property(
                    R7::class_data.frame,
                    getter = function(self){
                      c(
                        sprintf("0 %s SUBM %s", self@xref, self@text),
                        self@refs_ged,
                        sprintf("1 RIN %s", self@auto_id),
                        lst_to_ged(self@citations) |> increase_level(by = 1),
                        obj_to_ged(self@last_updated) |> increase_level(by = 1)
                      )
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


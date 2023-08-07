#' @include cls_validators.R
NULL

#' @include cls_change_date.R
class_record <- S7::new_class(
  "class_record", #abstract = TRUE,
  properties = list(
    confidential = S7::new_property(S7::class_logical, default = FALSE),
    locked = S7::new_property(S7::class_logical, default = FALSE),
    private = S7::new_property(S7::class_logical, default = FALSE),
    user_ids = S7::class_character, # potentially named
    unique_ids = S7::class_character, # not named
    external_ids = S7::class_character, # definitely named
    created = S7::class_character | class_creation_date,
    updated = S7::class_character | class_change_date,
    
    prim_uid = S7::new_property(
      S7::class_character,
      getter = function(self) unique_ids[1]
    ),
    
    restrictions = S7::new_property(S7::class_character,
                                    getter = function(self){
                                      conf <- rep("CONFIDENTIAL", self@confidential)
                                      lock <- rep("LOCKED", self@locked)
                                      priv <- rep("PRIVACY", self@private)
                                      
                                      toString(c(conf, lock, priv))
                                    }),
    
    ids = S7::new_property(S7::class_character,
                           getter = function(self){
                             c(
                               named_vec_to_ged(self@user_ids, "REFN", "TYPE"),
                               sprintf("0 UID %s", self@unique_ids),
                               named_vec_to_ged(self@external_ids, "EXID", "TYPE")
                             )
                           })
  ),
  validator = function(self){
    c(
      chk_input_size(self@confidential, "@confidential", 1, 1),
      chk_input_size(self@locked, "@locked", 1, 1),
      chk_input_size(self@private, "@private", 1, 1),
      chk_input_size(self@user_ids, "@user_ids", min_val = 1),
      chk_input_size(self@external_ids, "@external_ids", min_val = 1),
      chk_input_size(names(self@external_ids), "@external_ids types", min_val = 1),
      chk_input_size(self@created, "@created", 0, 1),
      chk_input_size(self@updated, "@updated", 0, 1),
      chk_input_pattern(self@unique_ids, "@unique_ids", reg_uuid(TRUE))
    )
  }
)



#' @export
#' @include cls_fact.R cls_non_event.R cls_association.R cls_note.R
#' cls_citation.R cls_media_link.R
class_record_fam <- S7::new_class(
  "class_record_fam", 
  package = "gedcomS7",
  parent = class_record,
  properties = list(
    facts = S7::class_list | class_fact_fam,
    non_events = S7::class_list | class_non_event,
    husb_uid = S7::class_character,
    wife_uid = S7::class_character,
    chil_uids = S7::class_character,
    associations = S7::class_list | class_association,
    subm_uids = S7::class_character,
    note_uids = S7::class_character,
    notes = S7::class_list | class_note | S7::class_character,
    citations = S7::class_list | class_citation | S7::class_character,
    media_links = S7::class_list | class_media_link | S7::class_character,
    
    relationship_date = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact == "MARR") return(fact@fact_date)
        }
        character()
      }),
    
    relationship_place = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact == "MARR") return(fact@fact_location)
        }
        character()
      }),
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s FAM", self@prim_uid),
          sprintf("1 RESN %s", self@restrictions),
          obj_to_ged(self@facts) |> increase_level(by = 1),
          obj_to_ged(self@non_events) |> increase_level(by = 1),
          named_vec_to_ged(self@husb_uid, "HUSB", "PHRASE") |> increase_level(by = 1),
          named_vec_to_ged(self@wife_uid, "WIFE", "PHRASE") |> increase_level(by = 1),
          named_vec_to_ged(self@chil_uids, "CHIL", "PHRASE") |> increase_level(by = 1),
          obj_to_ged(self@associations) |> increase_level(by = 1),
          sprintf("1 SUBM %s", self@subm_uids),
          self@ids |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_uids),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1),
          obj_to_ged(self@media_links, "OBJE") |> increase_level(by = 1),
          obj_to_ged(self@updated) |> increase_level(by = 1),
          obj_to_ged(self@created) |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      
      chk_input_size(self@husb_uid, "@husb_uid", 0, 1),
      chk_input_size(self@wife_uid, "@wife_uid", 0, 1),
      chk_input_pattern(self@husb_uid, "@husb_uid", reg_uuid(TRUE)),
      chk_input_pattern(self@wife_uid, "@wife_uid", reg_uuid(TRUE)),
      chk_input_pattern(self@chil_uids, "@chil_uids", reg_uuid(TRUE)),
      chk_input_pattern(self@subm_uids, "@subm_uids", reg_uuid(TRUE)),
      chk_input_pattern(self@note_uids, "@note_uids", reg_uuid(TRUE)),
      chk_input_S7classes(self@facts, "@facts", class_fact_fam),
      chk_input_S7classes(self@non_events, "@non_events", class_non_event),
      chk_input_S7classes(self@associations, "@associations", class_association),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+"),
      chk_input_S7classes(self@citations, "@citations", class_citation),
      chk_input_S7classes(self@media_links, "@media_links", class_media_link, reg_uuid(TRUE))
    )
  })

#' @export
#' @include cls_personal_name.R cls_fact.R cls_non_event.R cls_association.R cls_note.R
#' cls_citation.R cls_media_link.R
class_record_indi <- S7::new_class(
  "class_record_indi", 
  package = "gedcomS7",
  parent = class_record,
  properties = list(
    personal_names = S7::class_list | class_personal_name | S7::class_character,
    sex = S7::new_property(S7::class_character, default = "U"),
    facts = S7::class_list,
    non_events = S7::class_list,
    family_links_as_child = S7::class_list | class_child_family_link | S7::class_character,
    family_links_as_spouse = S7::class_list | class_spouse_family_link | S7::class_character,
    subm_uids = S7::class_character,
    associations = S7::class_list,
    alia_uids = S7::class_character,
    anci_uids = S7::class_character,
    desi_uids = S7::class_character,
    note_uids = S7::class_character,
    notes = S7::class_list | class_note | S7::class_character,
    citations = S7::class_list | class_citation | S7::class_character,
    media_links = S7::class_list | class_media_link | S7::class_character,
    
    primary_name = S7::new_property(
      S7::class_character,
      getter = function(self){
        if(length(self@personal_names) == 0){
          character()
        } else {
          self@personal_names[[1]]@name@full |>
            gsub(pattern = "/", replacement = "")
        }
      }),
    
    all_names = S7::new_property(
      S7::class_character,
      getter = function(self){
        sapply(self@personal_names, \(nm){
          gsub(nm@name@full, pattern = "/", replacement = "")
        }, USE.NAMES = FALSE)
      }),
    
    desc_short = S7::new_property(
      S7::class_character,
      getter = function(self){
        if(length(self@primary_name) == 0){
          name <- "Unnamed individual"
        } else {
          name <- self@primary_name
        }
        paste0("Individual ", self@xref, ", ", name)
      }),
    
    birth_date = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact == "BIRT") return(fact@fact_date)
        }
        character()
      }),
    
    birth_place = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact == "BIRT") return(fact@fact_location)
        }
        character()
      }),
    
    is_alive = S7::new_property(
      S7::class_logical,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact == "DEAT") return(FALSE)
        }
        TRUE
      }),
    
    death_date = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact == "DEAT") return(fact@fact_date)
        }
        character()
      }),
    
    death_place = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact == "DEAT") return(fact@fact_location)
        }
        character()
      }),
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s INDI", self@prim_uid),
          sprintf("1 RESN %s", self@restrictions),
          obj_to_ged(self@personal_names, "NAME") |> increase_level(by = 1),
          sprintf("1 SEX %s", self@sex),
          obj_to_ged(self@facts) |> increase_level(by = 1),
          obj_to_ged(self@non_events) |> increase_level(by = 1),
          obj_to_ged(self@family_links_as_child, "FAMC") |> increase_level(by = 1),
          obj_to_ged(self@family_links_as_spouse, "FAMS") |> increase_level(by = 1),
          sprintf("1 SUBM %s", self@subm_uids),
          obj_to_ged(self@associations) |> increase_level(by = 1),
          named_vec_to_ged(self@alia_uids, "ALIA", "PHRASE") |> increase_level(by = 1),
          sprintf("1 ANCI %s", self@anci_uids),
          sprintf("1 DESI %s", self@desi_uids),
          self@ids |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_uids),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1),
          obj_to_ged(self@media_links, "OBJE") |> increase_level(by = 1),
          obj_to_ged(self@updated) |> increase_level(by = 1),
          obj_to_ged(self@created) |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@sex, "@sex", 0, 1),
      chk_input_choice(self@sex, "@sex", val_sexes()),
      chk_input_pattern(self@subm_uids, "@subm_uids", reg_uuid(TRUE)),
      chk_input_pattern(self@alia_uids, "@alia_uids", reg_uuid(TRUE)),
      chk_input_pattern(self@anci_uids, "@anci_uids", reg_uuid(TRUE)),
      chk_input_pattern(self@desi_uids, "@desi_uids", reg_uuid(TRUE)),
      chk_input_pattern(self@note_uids, "@note_uids", reg_uuid(TRUE)),
      chk_input_S7classes(self@personal_names, "@personal_names", class_personal_name),
      chk_input_S7classes(self@facts, "@facts", class_fact_indi),
      chk_input_S7classes(self@non_events, "@non_events", class_non_event),
      chk_input_S7classes(self@family_links_as_child, "@family_links_as_child", class_child_family_link),
      chk_input_S7classes(self@family_links_as_spouse, "@family_links_as_spouse", class_spouse_family_link),
      chk_input_S7classes(self@associations, "@associations", class_association),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+"),
      chk_input_S7classes(self@citations, "@citations", class_citation),
      chk_input_S7classes(self@media_links, "@media_links", class_media_link, reg_uuid(TRUE))
    )
  }
)

#' @export
#' @include cls_media_file.R cls_note.R cls_citation.R
class_record_media <- S7::new_class(
  "class_record_media", 
  package = "gedcomS7",
  parent = class_record,
  properties = list(
    files = S7::class_list | class_media_file,
    note_uids = S7::class_character,
    notes = S7::class_list | class_note | S7::class_character,
    citations = S7::class_list | class_citation | S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s OBJE", self@prim_uid),
          sprintf("1 RESN %s", self@restrictions),
          obj_to_ged(self@files) |> increase_level(by = 1),
          self@ids |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_uids),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1),
          obj_to_ged(self@updated) |> increase_level(by = 1),
          obj_to_ged(self@created) |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_pattern(self@note_uids, "@note_uids", reg_uuid(TRUE)),
      chk_input_S7classes(self@files, "@files", class_media_file),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+"),
      chk_input_S7classes(self@citations, "@citations", class_citation)
    )
  }
)

#' @export
#' @include cls_address.R cls_note.R
class_record_repo <- S7::new_class(
  "class_record_repo", 
  package = "gedcomS7",
  parent = class_record,
  properties = list(
    name = S7::class_character,
    address = S7::class_character | class_address,
    phone_numbers = S7::class_character,
    emails = S7::class_character,
    faxes = S7::class_character,
    web_pages = S7::class_character,
    note_uids = S7::class_character,
    notes = S7::class_list | class_note | S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s REPO", self@prim_uid),
          sprintf("1 RESN %s", self@restrictions),
          sprintf("1 NAME %s", self@name),
          obj_to_ged(self@address) |> increase_level(by = 1),
          sprintf("1 PHON %s", self@phone_numbers),
          sprintf("1 EMAIL %s", self@emails),
          sprintf("1 FAX %s", self@faxes),
          sprintf("1 WWW %s", self@web_pages),
          sprintf("1 SNOTE %s", self@note_uids),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          self@ids |> increase_level(by = 1),
          obj_to_ged(self@updated) |> increase_level(by = 1),
          obj_to_ged(self@created) |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@name, "@name", 1, 1, 1),
      chk_input_size(self@address, "@address", 0, 1),
      chk_input_size(self@phone_numbers, "@phone_numbers", min_val = 1),
      chk_input_size(self@emails, "@emails", min_val = 1),
      chk_input_size(self@faxes, "@faxes", min_val = 1),
      chk_input_size(self@web_pages, "@web_pages", min_val = 1),
      chk_input_pattern(self@note_uids, "@note_uids", reg_uuid(TRUE)),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+")
    )
  }
)

#' @export
#' @include cls_translation.R cls_citation.R
class_record_note <- S7::new_class(
  "class_record_note", 
  package = "gedcomS7",
  parent = class_record,
  properties = list(
    text = S7::class_character,
    media_type = S7::class_character,
    language = S7::class_character,
    text_alt = S7::class_list | class_translation_txt,
    citations = S7::class_list | class_citation | S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_data.frame,
      getter = function(self){
        c(
          sprintf("0 %s SNOTE %s", self@prim_uid, self@text),
          sprintf("1 RESN %s", self@restrictions),
          sprintf("1 MIME %s", self@media_type),
          sprintf("1 LANG %s", self@language),
          obj_to_ged(self@text_alt) |> increase_level(by = 1),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1),
          self@ids |> increase_level(by = 1),
          obj_to_ged(self@updated) |> increase_level(by = 1),
          obj_to_ged(self@created) |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@text, "@text", 1, 1, 1),
      chk_input_size(self@language, "@language", 0, 1, 1),
      #TODO: language option
      chk_input_size(self@media_type, "@media_type", 0, 1, 1),
      #TODO: media type pattern (text/plain or text/html)
      chk_input_S7classes(self@text_alt, "@text_alt", class_translation_txt),
      chk_input_S7classes(self@citations, "@citations", class_citation)
    )
  }
)


#' @export
#' @include cls_events_recorded.R cls_note.R cls_translation.R cls_repository_citation.R
#' cls_note.R cls_media_link.R
class_record_sour <- S7::new_class(
  "class_record_sour", 
  package = "gedcomS7",
  parent = class_record,
  properties = list(
    events_recorded = S7::class_list | class_events_recorded | S7::class_character,
    responsible_agency = S7::class_character,
    data_note_uids = S7::class_character,
    data_notes = S7::class_list | class_note | S7::class_character,
    originator = S7::class_character,
    full_title = S7::class_character,
    short_title = S7::class_character,
    publication_facts = S7::class_character,
    source_text = NULL | class_translation_txt,
    repo_citations = S7::class_list | class_repository_citation | S7::class_character,
    note_uids = S7::class_character,
    notes = S7::class_list | class_note | S7::class_character,
    media_links = S7::class_list | class_media_link | S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s SOUR", self@prim_uid),
          sprintf("1 RESN %s", self@restrictions),
          rep("1 DATA", length(self@events_recorded) + length(self@responsible_agency) + 
                length(self@data_notes) + length(self@data_note_uids) > 0),
          obj_to_ged(self@events_recorded, "EVEN") |> increase_level(by = 2),
          sprintf("2 AGNC %s", self@responsible_agency),
          sprintf("2 SNOTE %s", self@data_note_uids),
          obj_to_ged(self@data_notes, "NOTE") |> increase_level(by = 2),
          sprintf("1 AUTH %s", self@originator),
          sprintf("1 TITL %s", self@full_title),
          sprintf("1 ABBR %s", self@short_title),
          sprintf("1 PUBL %s", self@publication_facts),
          obj_to_ged(self@source_text) |> increase_level(by = 1),
          obj_to_ged(self@repo_citations, "REPO") |> increase_level(by = 1),
          self@ids |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_uids),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          obj_to_ged(self@media_links, "OBJE") |> increase_level(by = 1),
          obj_to_ged(self@updated) |> increase_level(by = 1),
          obj_to_ged(self@created) |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@responsible_agency, "@responsible_agency", 0, 1, 1),
      chk_input_size(self@originator, "@originator", 0, 1, 1),
      chk_input_size(self@full_title, "@full_title", 0, 1, 1),
      chk_input_size(self@short_title, "@short_title", 0, 1, 1),
      chk_input_size(self@publication_facts, "@publication_facts", 0, 1, 1),
      chk_input_size(self@source_text, "@source_text", 0, 1),
      chk_input_pattern(self@data_note_uids, "@data_note_uids", reg_uuid(TRUE)),
      chk_input_pattern(self@note_uids, "@note_uids", reg_uuid(TRUE)),
      chk_input_S7classes(self@events_recorded, "@events_recorded", class_events_recorded),
      chk_input_S7classes(self@data_notes, "@data_notes", class_note, ".+"),
      chk_input_S7classes(self@repo_citations, "@repo_citations", class_repository_citation),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+"),
      chk_input_S7classes(self@media_links, "@media_links", class_media_link, reg_uuid(TRUE))
    )
  })



#' @export
#' @include cls_address.R cls_note.R cls_media_link.R
class_record_subm <- S7::new_class(
  "class_record_subm", 
  package = "gedcomS7",
  parent = class_record,
  properties = list(
    name = S7::class_character,
    address = S7::class_character | class_address,
    phone_numbers = S7::class_character,
    emails = S7::class_character,
    faxes = S7::class_character,
    web_pages = S7::class_character,
    media_links = S7::class_list | class_media_link | S7::class_character,
    language = S7::class_character,
    note_uids = S7::class_character,
    notes = S7::class_list | class_note | S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s SUBM", self@prim_uid),
          sprintf("1 RESN %s", self@restrictions),
          sprintf("1 NAME %s", self@name),
          obj_to_ged(self@address) |> increase_level(by = 1),
          sprintf("1 PHON %s", self@phone_numbers),
          sprintf("1 EMAIL %s", self@emails),
          sprintf("1 FAX %s", self@faxes),
          sprintf("1 WWW %s", self@web_pages),
          obj_to_ged(self@media_links, "OBJE") |> increase_level(by = 1),
          sprintf("1 LANG %s", self@language),
          self@ids |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_uids),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          obj_to_ged(self@updated) |> increase_level(by = 1),
          obj_to_ged(self@created) |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@name, "@name", 1, 1, 1),
      chk_input_size(self@address, "@address", 0, 1),
      chk_input_size(self@phone_numbers, "@phone_numbers", min_val = 1),
      chk_input_size(self@emails, "@emails", min_val = 1),
      chk_input_size(self@faxes, "@faxes", min_val = 1),
      chk_input_size(self@web_pages, "@web_pages", min_val = 1),
      #TODO: language pattern
      chk_input_pattern(self@note_uids, "@note_uids", reg_uuid(TRUE)),
      chk_input_S7classes(self@media_links, "@media_links", class_media_link, reg_uuid(TRUE)),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+")
    )
  }
)


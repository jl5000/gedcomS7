#' @include cls_validators.R
NULL


#' @include cls_date.R cls_place.R cls_address.R cls_association.R 
#' cls_note.R cls_citation.R cls_media_link.R
class_fact <- S7::new_class(
  "class_fact",
  properties = list(
    # Not part of detail, but want them to appear first
    fact = S7::class_character,
    description = S7::class_character,
    type = S7::class_character,
    
    date = NULL | class_date_value,
    place = NULL | class_place,
    address = NULL | class_address | S7::class_character,
    phone_numbers = S7::class_character,
    emails = S7::class_character,
    faxes = S7::class_character,
    web_pages = S7::class_character,
    agency = S7::class_character,
    relig_affil = S7::class_character,
    cause = S7::class_character,
    confidential = S7::new_property(S7::class_logical, default = FALSE),
    locked = S7::new_property(S7::class_logical, default = FALSE),
    private = S7::new_property(S7::class_logical, default = FALSE),
    sorting_date = NULL | class_date_value,
    associations = S7::class_list,
    note_uids = S7::class_character,
    notes = S7::class_list,
    citations = S7::class_list,
    media_links = S7::class_list,
    unique_ids = S7::class_character,
    
    restrictions = S7::new_property(S7::class_character,
                                    getter = function(self){
                                      conf <- rep("CONFIDENTIAL", self@confidential)
                                      lock <- rep("LOCKED", self@locked)
                                      priv <- rep("PRIVACY", self@private)
                                      
                                      toString(c(conf, lock, priv))
                                    }),
    
    fact_date = S7::new_property(
      S7::class_character,
      getter = function(self){
        date_to_val(self@date)
      }),
    
    fact_location = S7::new_property(
      S7::class_character,
      getter = function(self){
        if(length(self@place) == 1){
          self@place@as_val
        } else if(length(self@address) == 1) {
          self@address@as_val
        } else {
          character()
        }
      }),
    
    fact_detail_as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s %s", self@fact, chronify(self@description)),
          sprintf("1 TYPE %s", self@type),
          obj_to_ged(self@date) |> increase_level(by = 1),
          obj_to_ged(self@place) |> increase_level(by = 1),
          obj_to_ged(self@address) |> increase_level(by = 1),
          sprintf("1 PHON %s", self@phone_numbers),
          sprintf("1 EMAIL %s", self@emails),
          sprintf("1 FAX %s", self@faxes),
          sprintf("1 WWW %s", self@web_pages),
          sprintf("1 AGNC %s", self@agency),
          sprintf("1 RELI %s", self@relig_affil),
          sprintf("1 CAUS %s", self@cause),
          sprintf("1 RESN %s", self@restrictions),
          obj_to_ged(self@sorting_date) |> increase_level(by = 1),
          lst_to_ged(self@associations) |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_uids),
          lst_to_ged(self@notes) |> increase_level(by = 1),
          lst_to_ged(self@citations) |> increase_level(by = 1),
          lst_to_ged(self@media_links) |> increase_level(by = 1),
          sprintf("1 UID %s", self@unique_ids)
        )
      }
    )
  ),
  validator = function(self) {
    c(
      chk_input_size(self@fact, "@fact", 1, 1),
      chk_input_size(self@description, "@description", 0, 1),
      chk_input_size(self@type, "@type", 0, 1, 1),
      
      chk_input_size(self@date, "@date", 0, 1),
      chk_input_size(self@place, "@place", 0, 1),
      chk_input_size(self@address, "@address", 0, 1),
      chk_input_size(self@phone_numbers, "@phone_numbers", min_val = 1),
      chk_input_size(self@emails, "@emails", min_val = 1),
      chk_input_size(self@faxes, "@faxes", min_val = 1),
      chk_input_size(self@web_pages, "@web_pages", min_val = 1),
      chk_input_size(self@agency, "@agency", 0, 1, 1),
      chk_input_size(self@relig_affil, "@relig_affil", 0, 1, 1),
      chk_input_size(self@cause, "@cause", 0, 1, 1),
      chk_input_size(self@confidential, "@confidential", 1, 1),
      chk_input_size(self@locked, "@locked", 1, 1),
      chk_input_size(self@private, "@private", 1, 1),
      chk_input_size(self@sorting_date, "@sorting_date", 0, 1),
      chk_input_pattern(self@note_uids, "@note_uids", reg_uuid(TRUE)),
      chk_input_pattern(self@unique_uids, "@unique_uids", reg_uuid(TRUE)),
      chk_input_S7classes(self@associations, "@associations", class_association),
      chk_input_S7classes(self@notes, "@notes", class_note),
      chk_input_S7classes(self@citations, "@citations", class_citation),
      chk_input_S7classes(self@media_links, "@media_links", class_media_link)
    )
  }
)

class_fact_indi <- S7::new_class(
  "class_fact_indi", 
  parent = class_fact,
  properties = list(
    age = S7::class_character,
    age_phrase = S7::class_character,
    
    indi_fact_detail_as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        age <- self@age
        if(length(self@age_phrase) == 1)
          age <- chronify(self@age)
        
        c(
          self@fact_detail_as_ged,
          sprintf("1 AGE %s", age),
          sprintf("2 PHRASE %s", self@age_phrase)
        )
      }
    )
  ),
  validator = function(self){
    c(
      chk_input_size(self@age, "@age", 0, 1),
      chk_input_size(self@age_phrase, "@age_phrase", 0, 1, 1),
      chk_input_pattern(self@age, "@age", reg_age_at_event())
    )
  }
)

class_fact_fam <- S7::new_class(
  "class_fact_fam", 
  parent = class_fact,
  properties = list(
    husb_age = S7::class_character,
    husb_age_phrase = S7::class_character,
    wife_age = S7::class_character,
    wife_age_phrase = S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        husb_age <- self@husb_age
        if(length(self@husb_age_phrase) == 1)
          husb_age <- chronify(self@husb_age)
        
        wife_age <- self@wife_age
        if(length(self@wife_age_phrase) == 1)
          wife_age <- chronify(self@wife_age)
        
        ged <- c(
          self@fact_detail_as_ged,
          rep("1 HUSB", length(husb_age)),
          sprintf("2 AGE %s", husb_age),
          sprintf("3 PHRASE %s", self@husb_age_phrase),
          rep("1 WIFE", length(wife_age)),
          sprintf("2 AGE %s", wife_age),
          sprintf("3 PHRASE %s", self@wife_age_phrase)
        )
        
        if(self@fact %in% val_family_event_types(FALSE)){
          if(length(ged) == 1) return(sprintf("0 %s Y", self@fact))
        }
         
        ged 
      }
    )
  ),
  validator = function(self){
    c(
      chk_input_size(self@husb_age, "@husb_age", 0, 1),
      chk_input_size(self@husb_age_phrase, "@husb_age_phrase", 0, 1, 1),
      chk_input_size(self@wife_age, "@wife_age", 0, 1),
      chk_input_size(self@wife_age_phrase, "@wife_age_phrase", 0, 1, 1),
      chk_input_pattern(self@husb_age, "@husb_age", reg_age_at_event()),
      chk_input_pattern(self@wife_age, "@wife_age", reg_age_at_event())
    )
  }
)

#' @export
class_event_indi <- S7::new_class(
  "class_event_indi",
  package = "gedcomS7",
  parent = class_fact_indi,
  properties = list(
    fam_uid = S7::class_character,
    adop_parent = S7::class_character,
    adop_parent_phrase = S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        ged <- c(
          self@indi_fact_detail_as_ged,
          sprintf("1 FAMC %s", self@fam_uid),
          sprintf("2 ADOP %s", self@adop_parent),
          sprintf("3 PHRASE %s", self@adop_parent_phrase)
        )
        
        if(self@fact %in% val_individual_event_types(FALSE)){
          if(length(ged) == 1) return(sprintf("0 %s Y", self@fact))
        }
        
        ged
      })
  ),
  validator = function(self){
    c(
      chk_input_choice(self@fact, "@fact", val_individual_event_types(TRUE)),
      chk_input_parents(self@adop_parent_phrase, "@adop_parent_phrase", self@adop_parent, "@adop_parent"),
      #EVEN requires TYPE
      #BIRT and CHR may have a FAMC with no substructures; 
      # ADOP may have a FAMC with an optional ADOP substructure
      
      chk_input_size(self@fam_uid, "@fam_uid", 0, 1),
      chk_input_size(self@adop_parent, "@adop_parent", 0, 1),
      chk_input_size(self@adop_parent_phrase, "@adop_parent_phrase", 0, 1),
      chk_input_pattern(self@fam_uid, "@fam_uid", reg_uuid(TRUE)),
      chk_input_choice(self@adop_parent, "@adop_parent", val_adoptive_parents())
    )
  }
)

#' @export
class_attr_indi <- S7::new_class(
  "class_attr_indi",
  package = "gedcomS7",
  parent = class_fact_indi,
  properties = list(
    as_ged = S7::new_property(S7::class_character, 
                              getter = function(self) self@indi_fact_detail_as_ged)
  ),
  validator = function(self){
    c(
      chk_input_choice(self@fact, "@fact", val_individual_attribute_types(TRUE))
      #FACT/IDNO require TYPE, NCHI/NMR are integers
    )
  }
)

#' @export
class_event_fam <- S7::new_class(
  "class_event_fam",
  package = "gedcomS7",
  parent = class_fact_fam,
  validator = function(self){
    c(
      chk_input_choice(self@fact, "@fact", val_family_event_types(TRUE))
      #EVEN requires type
      #EVEN description is minchar1
    )
  }
)

#' @export
class_attr_fam <- S7::new_class(
  "class_attr_fam",
  package = "gedcomS7",
  parent = class_fact_fam,
  validator = function(self){
    c(
      chk_input_choice(self@fact, "@fact", val_family_attribute_types(TRUE))
      #FACT requires TYPE
    )
  }
)





#' 
#' 
#' #' @export
#' class_fact_fam <- S7::new_class(
#'   "class_fact_fam", 
#'   parent = class_fact_detail,
#'   properties = list(
#'     husband_age = S7::class_character,
#'     wife_age = S7::class_character,
#'     
#'     as_ged = S7::new_property(
#'       S7::class_character,
#'       getter = function(self){
#'         if(length(self@description) == 0){
#'           desc <- ""
#'         } else {
#'           desc <- paste0(" ", self@description)
#'         }
#'         ged <- c(
#'           sprintf("0 %s%s", self@fact, desc),
#'           rep("1 HUSB", length(self@husband_age)),
#'           sprintf("2 AGE %s", self@husband_age),
#'           rep("1 WIFE", length(self@wife_age)),
#'           sprintf("2 AGE %s", self@wife_age),
#'           sprintf("1 TYPE %s", self@type),
#'           sprintf("1 DATE %s", date_to_val(self@date)),
#'           obj_to_ged(self@place) |> increase_level(by = 1),
#'           obj_to_ged(self@address) |> increase_level(by = 1),
#'           sprintf("0 PHON %s", self@phone_numbers),
#'           sprintf("0 EMAIL %s", self@emails),
#'           sprintf("0 FAX %s", self@faxes),
#'           sprintf("0 WWW %s", self@web_pages),
#'           sprintf("1 AGNC %s", self@agency),
#'           sprintf("1 RELI %s", self@relig_affil),
#'           sprintf("1 CAUS %s", self@cause),
#'           sprintf("1 NOTE %s", self@note_links),
#'           sprintf("1 NOTE %s", self@notes),
#'           lst_to_ged(self@citations) |> increase_level(by = 1),
#'           sprintf("1 OBJE %s", self@media_links)
#'         )
#'         
#'         if(length(ged) == 1 && self@fact %in% c("MARR")){
#'           sprintf("0 %s Y", self@fact)
#'         } else {
#'           ged
#'         }
#'       })
#'   ),
#'   validator = function(self) {
#'     # Only EVEN needs description
#'     desc_error <- NULL
#'     if(self@fact == "MARR"){
#'       if(length(self@description) == 1 && self@description != "Y")
#'         desc_error <- "Invalid descriptor for marriage event"
#'     } else if(self@fact != "EVEN"){
#'       desc_error <- chk_input_size(self@description, "@description", 0, 0)
#'     }
#'     
#'     c(
#'       chk_input_size(self@fact, "@fact", 1, 1),
#'       chk_input_choice(self@fact, "@fact", val_family_event_types()),
#'       chk_input_size(self@description, "@description", 0, 1, 1, 90),
#'       desc_error,
#'       
#'     )
#'   }
#' )
#' 
#' #' @export
#' class_fact_indi <- S7::new_class(
#'   "class_fact_indi", 
#'   parent = class_fact_detail,
#'   properties = list(
#'     age = S7::class_character,
#'     fam_uid = S7::class_character,
#'     adopting_parent = S7::class_character,
#'     
#'     as_ged = S7::new_property(
#'       S7::class_character,
#'       getter = function(self){
#'         if(length(self@description) == 0){
#'           desc <- ""
#'         } else {
#'           desc <- paste0(" ", self@description)
#'         }
#'         ged <- c(
#'           sprintf("0 %s%s", self@fact, desc),
#'           sprintf("1 FAMC %s", self@fam_uid),
#'           sprintf("2 ADOP %s", self@adopting_parent),
#'           sprintf("1 AGE %s", self@age),
#'           sprintf("1 TYPE %s", self@type),
#'           sprintf("1 DATE %s", date_to_val(self@date)),
#'           obj_to_ged(self@place) |> increase_level(by = 1),
#'           obj_to_ged(self@address) |> increase_level(by = 1),
#'           sprintf("0 PHON %s", self@phone_numbers),
#'           sprintf("0 EMAIL %s", self@emails),
#'           sprintf("0 FAX %s", self@faxes),
#'           sprintf("0 WWW %s", self@web_pages),
#'           sprintf("1 AGNC %s", self@agency),
#'           sprintf("1 RELI %s", self@relig_affil),
#'           sprintf("1 CAUS %s", self@cause),
#'           sprintf("1 NOTE %s", self@note_links),
#'           sprintf("1 NOTE %s", self@notes),
#'           lst_to_ged(self@citations) |> increase_level(by = 1),
#'           sprintf("1 OBJE %s", self@media_links)
#'         )
#'         
#'         if(length(ged) == 1 && self@fact %in% c("CHR","DEAT")){
#'           sprintf("0 %s Y", self@fact)
#'         } else {
#'           ged
#'         }
#'       })
#'   ),
#'   validator = function(self) {
#'     # Some facts (do not) require descriptions
#'     fact_desc_error <- NULL
#'     if(self@fact %in% val_attribute_types()){
#'       if(self@fact == "RESI"){
#'         fact_desc_error <- chk_input_size(self@description, "@description", 0, 0)
#'       } else {
#'         fact_desc_error <- chk_input_size(self@description, "@description", 1, 1)
#'       }
#'     } else if(self@fact %in% val_individual_event_types()){
#'       if(self@fact %in% c("CHR","DEAT","EVEN")){
#'         if(self@fact %in% c("CHR","DEAT") && length(self@description) == 1 && self@description != "Y"){
#'           fact_desc_error <- "Invalid descriptor for christening/death event"
#'         }
#'       } else {
#'         fact_desc_error <- chk_input_size(self@description, "@description", 0, 0)
#'       }
#'     }
#'     
#'     # Some facts require types
#'     fact_type_error <- NULL
#'     if(self@fact %in% c("IDNO","FACT"))
#'       fact_type_error <- chk_input_size(self@type, "@type", 1, 1)
#'     
#'     # fam uid only used for birth, christening, adoption
#'     fam_uid_error <- NULL
#'     if(!self@fact %in% c("BIRT","CHR","ADOP"))
#'       fam_uid_error <- chk_input_size(self@fam_uid, "@fam_uid", 0, 0)
#'     
#'     # adoptive parent only used for adoption with fam uid
#'     adop_par_error <- NULL
#'     if(self@fact != "ADOP" || length(self@fam_uid) == 0)
#'       adop_par_error <- chk_input_size(self@adopting_parent, "@adopting_parent", 0, 0)
#'     
#' 
#'     c(
#'       chk_input_size(self@fact, "@fact", 1, 1),
#'       chk_input_choice(self@fact, "@fact", c(val_attribute_types(), val_individual_event_types())),
#'       chk_input_size(self@description, "@description", 0, 1, 1, desc_max_char),
#'       fact_desc_error,
#'       fact_type_error,
#'       fam_uid_error,
#'       chk_input_size(self@fam_uid, "@fam_uid", 0, 1, 3, 22),
#'       
#'       adop_par_error,
#'       
#'       chk_input_size(self@age, "@age", 0, 1, 2, 13),
#'       chk_input_pattern(self@age, "@age", reg_age_at_event())
#'     )
#'   }
#' )
#' 

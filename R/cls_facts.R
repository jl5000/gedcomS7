#' @include cls_validators.R
NULL

#' @export
#' @include cls_dates.R cls_common.R
class_non_event <- S7::new_class(
  "class_non_event",
  properties = list(
    event = S7::class_character,
    date_period = S7::new_property(S7::new_union(NULL, class_date_period, S7::class_character)),
    date_phrase = S7::class_character,
    note_links = S7::class_character,
    notes = S7::new_property(S7::new_union(S7::class_character, S7::class_list)),
    citations = S7::new_property(S7::new_union(S7::class_character, S7::class_list)),
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@event, "@event", 1, 1)
      #TODO: event choice
      
    )
  }
)

class_fact_detail <- S7::new_class(
  "class_fact_detail",
  properties = list(
    # Not part of detail, but want them to appear first
    fact = S7::class_character,
    description = S7::class_character,
    
    type = S7::class_character,
    date = S7::new_property(S7::new_union(NULL, 
                                          class_date_calendar,
                                          class_date_period,
                                          class_date_range,
                                          class_date_approx, 
                                          S7::class_character)),
    time = S7::new_property(S7::new_union(NULL, class_time, S7::class_character)),
    place = S7::new_property(S7::new_union(NULL, class_place)),
    address = S7::new_property(S7::new_union(NULL, class_address)),
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
    note_links = S7::class_character,
    notes = S7::new_property(S7::new_union(S7::class_character, S7::class_list)),
    citations = S7::class_list,
    media_links = S7::class_character,
    
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
      })
  ),
  validator = function(self) {
    c(
      chk_input_size(self@type, "@type", 0, 1, 1, 90),
      chk_input_size(self@date, "@date", 0, 1, 1, 35),
      chk_input_pattern(self@date, "@date", reg_date_value()),
      chk_input_size(self@place, "@place", 0, 1),
      chk_input_size(self@address, "@address", 0, 1),
      chk_input_size(self@phone_numbers, "@phone_numbers", 0, 3, 1, 90),
      chk_input_size(self@emails, "@emails", 0, 3, 5, 120),
      chk_input_size(self@faxes, "@faxes", 0, 3, 5, 60),
      chk_input_size(self@web_pages, "@web_pages", 0, 3, 4, 2047),
      chk_input_size(self@agency, "@agency", 0, 1, 1, 120),
      chk_input_size(self@relig_affil, "@relig_affil", 0, 1, 1, 90),
      chk_input_size(self@cause, "@cause", 0, 1, 1, 90),
      chk_input_size(self@confidential, "@confidential", 1, 1),
      chk_input_size(self@locked, "@locked", 1, 1),
      chk_input_size(self@private, "@private", 1, 1),
      chk_input_size(self@note_links, "@note_links", 0, 10000, 3, 22),
      chk_input_pattern(self@note_links, "@note_links", reg_xref(TRUE)),
      chk_input_size(self@notes, "@notes", 0, 10000, 1, 32767),
      chk_input_S7classes(self@citations, "@citations", class_citation),
      chk_input_size(self@media_links, "@media_links", 0, 10000, 3, 22),
      chk_input_pattern(self@media_links, "@media_links", reg_xref(TRUE))
    )
  }
)

#' @export
class_fact_fam <- S7::new_class(
  "class_fact_fam", 
  parent = class_fact_detail,
  properties = list(
    husband_age = S7::class_character,
    wife_age = S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        if(length(self@description) == 0){
          desc <- ""
        } else {
          desc <- paste0(" ", self@description)
        }
        ged <- c(
          sprintf("0 %s%s", self@fact, desc),
          rep("1 HUSB", length(self@husband_age)),
          sprintf("2 AGE %s", self@husband_age),
          rep("1 WIFE", length(self@wife_age)),
          sprintf("2 AGE %s", self@wife_age),
          sprintf("1 TYPE %s", self@type),
          sprintf("1 DATE %s", date_to_val(self@date)),
          obj_to_ged(self@place) |> increase_level(by = 1),
          obj_to_ged(self@address) |> increase_level(by = 1),
          sprintf("0 PHON %s", self@phone_numbers),
          sprintf("0 EMAIL %s", self@emails),
          sprintf("0 FAX %s", self@faxes),
          sprintf("0 WWW %s", self@web_pages),
          sprintf("1 AGNC %s", self@agency),
          sprintf("1 RELI %s", self@relig_affil),
          sprintf("1 CAUS %s", self@cause),
          sprintf("1 NOTE %s", self@note_links),
          sprintf("1 NOTE %s", self@notes),
          lst_to_ged(self@citations) |> increase_level(by = 1),
          sprintf("1 OBJE %s", self@media_links)
        )
        
        if(length(ged) == 1 && self@fact %in% c("MARR")){
          sprintf("0 %s Y", self@fact)
        } else {
          ged
        }
      })
  ),
  validator = function(self) {
    # Only EVEN needs description
    desc_error <- NULL
    if(self@fact == "MARR"){
      if(length(self@description) == 1 && self@description != "Y")
        desc_error <- "Invalid descriptor for marriage event"
    } else if(self@fact != "EVEN"){
      desc_error <- chk_input_size(self@description, "@description", 0, 0)
    }
    
    c(
      chk_input_size(self@fact, "@fact", 1, 1),
      chk_input_choice(self@fact, "@fact", val_family_event_types()),
      chk_input_size(self@description, "@description", 0, 1, 1, 90),
      desc_error,
      chk_input_size(self@husband_age, "@husband_age", 0, 1, 2, 13),
      chk_input_size(self@wife_age, "@wife_age", 0, 1, 2, 13),
      chk_input_pattern(self@husband_age, "@husband_age", reg_age_at_event()),
      chk_input_pattern(self@wife_age, "@wife_age", reg_age_at_event())
    )
  }
)

#' @export
class_fact_indi <- S7::new_class(
  "class_fact_indi", 
  parent = class_fact_detail,
  properties = list(
    age = S7::class_character,
    fam_uid = S7::class_character,
    adopting_parent = S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        if(length(self@description) == 0){
          desc <- ""
        } else {
          desc <- paste0(" ", self@description)
        }
        ged <- c(
          sprintf("0 %s%s", self@fact, desc),
          sprintf("1 FAMC %s", self@fam_uid),
          sprintf("2 ADOP %s", self@adopting_parent),
          sprintf("1 AGE %s", self@age),
          sprintf("1 TYPE %s", self@type),
          sprintf("1 DATE %s", date_to_val(self@date)),
          obj_to_ged(self@place) |> increase_level(by = 1),
          obj_to_ged(self@address) |> increase_level(by = 1),
          sprintf("0 PHON %s", self@phone_numbers),
          sprintf("0 EMAIL %s", self@emails),
          sprintf("0 FAX %s", self@faxes),
          sprintf("0 WWW %s", self@web_pages),
          sprintf("1 AGNC %s", self@agency),
          sprintf("1 RELI %s", self@relig_affil),
          sprintf("1 CAUS %s", self@cause),
          sprintf("1 NOTE %s", self@note_links),
          sprintf("1 NOTE %s", self@notes),
          lst_to_ged(self@citations) |> increase_level(by = 1),
          sprintf("1 OBJE %s", self@media_links)
        )
        
        if(length(ged) == 1 && self@fact %in% c("CHR","DEAT")){
          sprintf("0 %s Y", self@fact)
        } else {
          ged
        }
      })
  ),
  validator = function(self) {
    # Some facts (do not) require descriptions
    fact_desc_error <- NULL
    if(self@fact %in% val_attribute_types()){
      if(self@fact == "RESI"){
        fact_desc_error <- chk_input_size(self@description, "@description", 0, 0)
      } else {
        fact_desc_error <- chk_input_size(self@description, "@description", 1, 1)
      }
    } else if(self@fact %in% val_individual_event_types()){
      if(self@fact %in% c("CHR","DEAT","EVEN")){
        if(self@fact %in% c("CHR","DEAT") && length(self@description) == 1 && self@description != "Y"){
          fact_desc_error <- "Invalid descriptor for christening/death event"
        }
      } else {
        fact_desc_error <- chk_input_size(self@description, "@description", 0, 0)
      }
    }
    
    # Some facts require types
    fact_type_error <- NULL
    if(self@fact %in% c("IDNO","FACT"))
      fact_type_error <- chk_input_size(self@type, "@type", 1, 1)
    
    # fam uid only used for birth, christening, adoption
    fam_uid_error <- NULL
    if(!self@fact %in% c("BIRT","CHR","ADOP"))
      fam_uid_error <- chk_input_size(self@fam_uid, "@fam_uid", 0, 0)
    
    # adoptive parent only used for adoption with fam uid
    adop_par_error <- NULL
    if(self@fact != "ADOP" || length(self@fam_uid) == 0)
      adop_par_error <- chk_input_size(self@adopting_parent, "@adopting_parent", 0, 0)
    
    desc_max_char <- 90
    if(self@fact %in% c("NMR","NCHI")) desc_max_char <- 3
    if(self@fact %in% c("IDNO")) desc_max_char <- 30
    if(self@fact %in% c("CAST","RELI","OCCU")) desc_max_char <- 90
    if(self@fact %in% c("NATI","TITL")) desc_max_char <- 120
    if(self@fact %in% c("EDUC","PROP")) desc_max_char <- 248
    if(self@fact %in% c("DSCR")) desc_max_char <- 4095
    
    c(
      chk_input_size(self@fact, "@fact", 1, 1),
      chk_input_choice(self@fact, "@fact", c(val_attribute_types(), val_individual_event_types())),
      chk_input_size(self@description, "@description", 0, 1, 1, desc_max_char),
      fact_desc_error,
      fact_type_error,
      fam_uid_error,
      chk_input_size(self@fam_uid, "@fam_uid", 0, 1, 3, 22),
      chk_input_pattern(self@fam_uid, "@fam_uid", reg_xref(TRUE)),
      adop_par_error,
      chk_input_size(self@adopting_parent, "@adopting_parent", 0, 1, 4, 4),
      chk_input_choice(self@adopting_parent, "@adopting_parent", val_adoptive_parents()),
      chk_input_size(self@age, "@age", 0, 1, 2, 13),
      chk_input_pattern(self@age, "@age", reg_age_at_event())
    )
  }
)


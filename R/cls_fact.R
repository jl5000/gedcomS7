#' @include cls_validators.R
NULL


#' Create a fact object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM EVENT_DETAIL (plus a bit more).
#' @include cls_date.R cls_place.R cls_address.R cls_association.R 
#' cls_note.R cls_citation.R cls_media_link.R
class_fact <- S7::new_class(
  "class_fact",
  abstract = TRUE,
  properties = list(
    # Not part of detail, but want them to appear first
    fact_type = S7::new_property(S7::class_character,
                                 validator = function(value){
                                   chk_input_size(value, 1, 1)
                                   # fact_type enum checked later
                                 }),
    fact_val = S7::new_property(S7::class_character,
                                validator = function(value){
                                  chk_input_size(value, 0, 1, 1)
                                }),
    fact_desc = S7::new_property(S7::class_character,
                                 validator = function(value){
                                   chk_input_size(value, 0, 1, 1)
                                 }),
    
    date = S7::new_property(S7::class_character | class_date_value,
                            validator = function(value){
                              c(
                                chk_input_size(value, 0, 1),
                                chk_input_pattern(value, reg_date_value())
                              )
                            }),
    place = S7::new_property(S7::class_character | class_place,
                             validator = function(value){
                               chk_input_size(value, 0, 1, 1)
                             }),
    address = S7::new_property(S7::class_character | class_address,
                               validator = function(value){
                                 chk_input_size(value, 0, 1, 1)
                               }),
    phone_numbers = S7::new_property(S7::class_character,
                                     validator = function(value){
                                       chk_input_size(value, min_val = 1)
                                     }),
    emails = S7::new_property(S7::class_character,
                              validator = function(value){
                                chk_input_size(value, min_val = 1)
                              }),
    faxes = S7::new_property(S7::class_character,
                             validator = function(value){
                               chk_input_size(value, min_val = 1)
                             }),
    web_pages = S7::new_property(S7::class_character,
                                 validator = function(value){
                                   chk_input_size(value, min_val = 1)
                                 }),
    agency = S7::new_property(S7::class_character,
                              validator = function(value){
                                chk_input_size(value, 0, 1, 1)
                              }),
    relig_affil = S7::new_property(S7::class_character,
                                   validator = function(value){
                                     chk_input_size(value, 0, 1, 1)
                                   }),
    cause = S7::new_property(S7::class_character,
                             validator = function(value){
                               chk_input_size(value, 0, 1, 1)
                             }),
    confidential = S7::new_property(S7::class_logical, default = FALSE,
                                    validator = function(value){
                                      chk_input_size(value, 1, 1)
                                    }),
    locked = S7::new_property(S7::class_logical, default = FALSE,
                              validator = function(value){
                                chk_input_size(value, 1, 1)
                              }),
    private = S7::new_property(S7::class_logical, default = FALSE,
                               validator = function(value){
                                 chk_input_size(value, 1, 1)
                               }),
    date_sort = S7::new_property(S7::class_character | class_date_sort,
                                 validator = function(value){
                                   c(
                                     chk_input_size(value, 0, 1),
                                     chk_input_pattern(value, reg_date_gregorian())
                                   )
                                 }),
    associations = S7::new_property(S7::class_list | class_association,
                                    validator = function(value){
                                      chk_input_S7classes(value, class_association)
                                    }),
    note_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    notes = S7::new_property(S7::class_list | class_note | S7::class_character,
                             validator = function(value){
                               chk_input_S7classes(value, class_note, ".+")
                             }),
    citations = S7::new_property(S7::class_list | class_citation | S7::class_character,
                                 validator = function(value){
                                   chk_input_S7classes(value, class_citation, reg_xref(TRUE))
                                 }),
    media_links = S7::new_property(S7::class_list | class_media_link | S7::class_character,
                                   validator = function(value){
                                     chk_input_S7classes(value, class_media_link, reg_xref(TRUE))
                                   }),
    unique_ids = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_uuid(TRUE))
                                  }),
    
    restrictions = S7::new_property(S7::class_character,
                                    getter = function(self){
                                      if(sum(self@confidential, self@locked, self@private) == 0)
                                        return(character())
                                      
                                      conf <- rep("CONFIDENTIAL", self@confidential)
                                      lock <- rep("LOCKED", self@locked)
                                      priv <- rep("PRIVACY", self@private)
                                      
                                      toString(c(conf, lock, priv))
                                    }),
    
    fact_date = S7::new_property(
      S7::class_character,
      getter = function(self){
        obj_to_val(self@date)
      }),
    
    fact_location = S7::new_property(
      S7::class_character,
      getter = function(self){
        want_addr <- self@fact_type %in% c("RESI","CENS","PROP")
        if(want_addr && length(self@address) == 1){
          return(obj_to_val(self@address))
        }
          
        if(length(self@place) == 1){
          obj_to_val(self@place)
        } else if(length(self@address) == 1) {
          obj_to_val(self@address) |> 
            strsplit("; ") |> 
            unlist() |> 
            tail(2) |> 
            paste(collapse = ", ")
        } else {
          character()
        }
      }),
    
    .fact_detail_as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        # if(self@fact_type %in% val_event_types(FALSE) &&
        #    length(self@fact_val) == 0 && 
        #    length(self@date) == 0 &&
        #    length(self@place) == 0){
        #   warning(sprintf("You are asserting that this %s event did not necessarily occur. Either define a date, place, or set @fact_val = 'Y' to assert its occurence.",
        #                   self@fact_type))
        # }
        
        c(
          obj_to_ged(self@date, "DATE") |> increase_level(by = 1),
          obj_to_ged(self@place, "PLAC") |> increase_level(by = 1),
          obj_to_ged(self@address, "ADDR") |> increase_level(by = 1),
          sprintf("1 PHON %s", self@phone_numbers),
          sprintf("1 EMAIL %s", self@emails),
          sprintf("1 FAX %s", self@faxes),
          sprintf("1 WWW %s", self@web_pages),
          sprintf("1 AGNC %s", self@agency),
          sprintf("1 RELI %s", self@relig_affil),
          sprintf("1 CAUS %s", self@cause),
          sprintf("1 RESN %s", self@restrictions),
          obj_to_ged(self@date_sort, "SDATE") |> increase_level(by = 1),
          obj_to_ged(self@associations) |> increase_level(by = 1),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1),
          obj_to_ged(self@media_links, "OBJE") |> increase_level(by = 1),
          sprintf("1 UID %s", self@unique_ids)
        )
      }
    )
  ),
  validator = function(self) {
    if(self@fact_type %in% val_event_types(FALSE) && 
       length(self@fact_val) == 1 && 
       self@fact_val != "Y")
      return("Only a @fact_val of 'Y' is permitted for this event.")
    
    if(self@fact_type %in% c("EVEN", val_attribute_types(TRUE)) && 
       self@fact_type != "RESI" &&
       length(self@fact_val) == 0)
      return("A @fact_val is required for this fact.")
    
    if(self@fact_type %in% c("FACT","EVEN","IDNO") && length(self@fact_desc) == 0)
      return("A @fact_desc is required for this type of fact.")
  }
)

extract_common_fact_elements <- function(fact, lines){
  
  tag <- extract_ged_tag(lines[1])
  
  S7::props(fact) <- list(
    date = extract_date_value(lines, tag),
    place = extract_place(lines, tag),
    address = extract_address(lines, tag),
    phone_numbers = find_ged_values(lines, c(tag, "PHON")),
    emails = find_ged_values(lines, c(tag, "EMAIL")),
    faxes = find_ged_values(lines, c(tag, "FAX")),
    web_pages = find_ged_values(lines, c(tag, "WWW")),
    agency = find_ged_values(lines, c(tag, "AGNC")),
    relig_affil = find_ged_values(lines, c(tag, "RELI")),
    cause = find_ged_values(lines, c(tag, "CAUS")),
    date_sort = extract_date_value(lines, tag, sorting = TRUE),
    associations = extract_associations(lines, tag),
    note_xrefs = find_ged_values(lines, c(tag, "SNOTE")),
    notes = extract_notes(lines, tag),
    citations = extract_citations(lines, tag),
    media_links = extract_media_links(lines, tag),
    unique_ids = find_ged_values(lines, c(tag, "UID"))
  )
  
  resn <- find_ged_values(lines, c(tag, "RESN"))
  if(length(resn) > 0){
    S7::props(fact) <- list(
      locked = grepl("LOCKED", resn),
      confidential = grepl("CONFIDENTIAL", resn),
      private = grepl("PRIVATE", resn)
    )
  }
  
  fact
}

Fact <- S7::new_class(
  "Fact",
  abstract = TRUE,
  properties = list(
    # Not part of detail, but want them to appear first
    fact_type = S7::new_property(S7::class_character,
                                 validator = function(value){
                                   chk_input_size(value, 1, 1)
                                   # fact_type enum checked later
                                 }),
    fact_val = S7::new_property(S7::class_character,
                                getter = function(self) self@fact_val,
                                setter = function(self, value){
                                  self@fact_val <- as.character(value)
                                  self
                                },
                                validator = function(value){
                                  chk_input_size(value, 0, 1, 1)
                                }),
    fact_desc = S7::new_property(S7::class_character,
                                 validator = function(value){
                                   chk_input_size(value, 0, 1, 1)
                                 }),
    
    date = S7::new_property(S7::class_character | 
                              S7::new_S3_class("gedcomS7::DateValue"),
                            getter = function(self) self@date,
                            setter = function(self, value){
                              if(is.character(value)) value <- toupper(value)
                              self@date <- value
                              self
                            },
                            validator = function(value){
                              c(
                                chk_input_size(value, 0, 1),
                                chk_input_pattern(value, reg_date_value())
                              )
                            }),
    place = S7::new_property(NULL | S7::new_S3_class("gedcomS7::Place"),
                             getter = function(self) self@place,
                             setter = function(self, value){
                               self@place <- as.S7class(value, gedcomS7::Place)
                               self
                             }),
    address = S7::new_property(NULL | S7::new_S3_class("gedcomS7::Address"),
                               getter = function(self) self@address,
                               setter = function(self, value){
                                 self@address <- as.S7class(value, gedcomS7::Address)
                                 self
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
    date_sort = S7::new_property(S7::class_character | 
                                   S7::new_S3_class("gedcomS7::DateSorting"),
                                 validator = function(value){
                                   c(
                                     chk_input_size(value, 0, 1),
                                     chk_input_pattern(value, reg_date_gregorian())
                                   )
                                 }),
    associations = S7::new_property(S7::class_list,
                                    getter = function(self) self@associations,
                                    setter = function(self, value){
                                      self@associations <- as.S7class_list(value, gedcomS7::Association)
                                      self
                                    },
                                    validator = function(value){
                                      for(inp in value) if(is.character(inp)) return(inp)
                                    }),
    note_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    notes = S7::new_property(S7::class_list,
                             getter = function(self) self@notes,
                             setter = function(self, value){
                               self@notes <- as.S7class_list(value, gedcomS7::Note)
                               self
                             },
                             validator = function(value){
                               for(inp in value) if(is.character(inp)) return(inp)
                             }),
    citations = S7::new_property(S7::class_list,
                                 getter = function(self) self@citations,
                                 setter = function(self, value){
                                   self@citations <- as.S7class_list(value, gedcomS7::SourceCitation)
                                   self
                                 },
                                 validator = function(value){
                                   for(inp in value) if(is.character(inp)) return(inp)
                                 }),
    media_links = S7::new_property(S7::class_list,
                                   getter = function(self) self@media_links,
                                   setter = function(self, value){
                                     self@media_links <- as.S7class_list(value, gedcomS7::MediaLink)
                                     self
                                   },
                                   validator = function(value){
                                     for(inp in value) if(is.character(inp)) return(inp)
                                   }),
    unique_ids = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_uuid(TRUE))
                                  }),
    
    c_restrictions = S7::new_property(S7::class_character,
                                    getter = function(self){
                                      restrictions_to_resn(self@confidential, self@locked, self@private)
                                    }),
    
    c_fact_date = S7::new_property(
      S7::class_character,
      getter = function(self){
        obj_to_val(self@date)
      }),
    
    c_fact_location = S7::new_property(
      S7::class_character,
      getter = function(self){
        want_addr <- self@fact_type %in% c("RESI","CENS","PROP")
        if(want_addr && length(self@address) == 1){
          return(obj_to_val(self@address))
        }
        
        if(length(self@address) == 1 &&
           length(self@address@city) + 
           length(self@address@state) + 
           length(self@address@country) > 1){
          toString(c(self@address@city, self@address@state, self@address@country))
        } else if(length(self@place) == 1){
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
          sprintf("1 RESN %s", self@c_restrictions),
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
    rules <- fact_rules_df()
    relev_rules <- rules[rules$fact_type == self@fact_type,]
    if(nrow(relev_rules) == 0) return()
    
    if(relev_rules$fact_val_required && length(self@fact_val) == 0)
      return("A @fact_val is required for this fact.")
    
    if(length(self@fact_val) == 1 && relev_rules$fact_val != "Any"){
      if(relev_rules$fact_val == "Y"){
        if(self@fact_val != "Y")
          return("Only a @fact_val of 'Y' is permitted for this event.")
      } else if(relev_rules$fact_val == "Integer"){
        if(!grepl("^\\d+$", self@fact_val))
          return("Number of children/marriages must be a whole number.")
      }
    }
    
    if(relev_rules$fact_desc_required && length(self@fact_desc) == 0)
      return("A @fact_desc is required for this type of fact.")
    
    if(is.character(self@date) && isTRUE(self@date == ""))
      return("A blank @date requires a @date_phrase and therefore requires a DateValue object.")
  }
)


#' Property requirements for fact objects.
#'
#' @returns A dataframe detailing the property requirements for each type of fact.
#' @export
fact_rules_df <- function(){
  
  all_df <- data.frame(fact_name = names(val_fact_types(TRUE)),
                       fact_type = unname(val_fact_types(TRUE)))
  
  # General rules
  all_df$individual <- all_df$fact_type %in% c(val_individual_attribute_types(TRUE),
                                               val_individual_event_types(TRUE))
  all_df$family <- all_df$fact_type %in% c(val_family_attribute_types(TRUE),
                                           val_family_event_types(TRUE))
  all_df$fact <- ifelse(all_df$fact_type %in% c(val_family_event_types(TRUE),
                                                val_individual_event_types(TRUE)),
                        "Event", "Attribute")
  all_df$fact_val_required <- all_df$fact == "Attribute"
  all_df$fact_val <- ifelse(all_df$fact == "Event", "Y", "Any")
  
  # Exceptions
  all_df$fact_val <- ifelse(all_df$fact_type %in% c("NCHI","NMR"),
                            "Integer", all_df$fact_val)
  all_df$fact_val_required <- ifelse(all_df$fact_type %in% c("FACT","EVEN"),
                                     TRUE, all_df$fact_val_required)
  all_df$fact_val_required <- ifelse(all_df$fact_type %in% c("RESI"),
                                     FALSE, all_df$fact_val_required)
  all_df$fact_val <- ifelse(all_df$fact_type %in% c("FACT","EVEN"),
                            "Any", all_df$fact_val)
  all_df$fact_desc_required <- all_df$fact_type %in% c("FACT", "EVEN", "IDNO")
  
  all_df
}



parse_common_fact_elements <- function(fact, lines){
  
  tag <- parse_line_tag(lines[1])
  
  S7::props(fact) <- list(
    date = parse_date_value(lines, tag),
    place = parse_place(lines, tag),
    address = parse_address(lines, tag),
    phone_numbers = find_ged_values(lines, c(tag, "PHON")),
    emails = find_ged_values(lines, c(tag, "EMAIL")),
    faxes = find_ged_values(lines, c(tag, "FAX")),
    web_pages = find_ged_values(lines, c(tag, "WWW")),
    agency = find_ged_values(lines, c(tag, "AGNC")),
    relig_affil = find_ged_values(lines, c(tag, "RELI")),
    cause = find_ged_values(lines, c(tag, "CAUS")),
    date_sort = parse_date_value(lines, tag, sorting = TRUE),
    associations = parse_associations(lines, tag),
    note_xrefs = find_ged_values(lines, c(tag, "SNOTE")),
    notes = parse_notes(lines, tag),
    citations = parse_citations(lines, tag),
    media_links = parse_media_links(lines, tag),
    unique_ids = find_ged_values(lines, c(tag, "UID"))
  )
  
  resn <- find_ged_values(lines, c(tag, "RESN"))
  if(length(resn) > 0){
    S7::props(fact) <- list(
      locked = grepl("LOCKED", resn),
      confidential = grepl("CONFIDENTIAL", resn),
      private = grepl("PRIVACY", resn)
    )
  }
  
  fact
}

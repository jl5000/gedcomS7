
Fact <- S7::new_class(
  "Fact",
  parent = GedcomS7class,
  abstract = TRUE,
  properties = list(
    # Not part of detail, but want them to appear first
    fact_type = prop_char(1, 1), # fact_type enum checked later
    fact_val = prop_char(0, 1, 1, casting_name = "fact_val"),
    fact_desc = prop_char(0, 1, 1),
    
    date = prop_char(0, 1, pattern = reg_date_value(), S7class_names = "DateValue"),
    place = prop_S7obj("place", Place),
    address = prop_S7obj("address", Address),
    phone_numbers = prop_char(min_char = 1),
    emails = prop_char(min_char = 1),
    faxes = prop_char(min_char = 1),
    web_pages = prop_char(min_char = 1),
    agency = prop_char(0, 1, 1),
    relig_affil = prop_char(0, 1, 1),
    cause = prop_char(0, 1, 1),
    confidential = prop_bool(default = FALSE),
    locked = prop_bool(default = FALSE),
    private = prop_bool(default = FALSE),
    date_sort = prop_char(0, 1, pattern = reg_date_gregorian(), S7class_names = "DateSorting"),
    associations = prop_S7list("associations", Association),
    note_xrefs = prop_char(pattern = reg_xref(TRUE)),
    notes = prop_S7list("notes", Note),
    citations = prop_S7list("citations", SourceCitation),
    media_links = prop_S7list("media_links", MediaLink),
    unique_ids = prop_char(pattern = reg_uuid(TRUE)),

    FACT_DATE = S7::new_property(
      S7::class_character,
      getter = function(self){
        obj_to_val(self@date)
      }),
    
    FACT_LOCATION = S7::new_property(
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
          sprintf("1 RESN %s", restrictions_to_resn(self@confidential, self@locked, self@private)),
          obj_to_ged(self@date_sort, "SDATE") |> increase_level(by = 1),
          obj_to_ged(self@associations) |> increase_level(by = 1),
          notes_to_ged(self@notes, self@note_xrefs) |> increase_level(by = 1),
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
#' @param type A character string to filter the table.
#'
#' @returns A dataframe detailing the property requirements for each type of fact.
#' @export
fact_rules_df <- function(type = NULL){
  
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
  
  if(!is.null(type)){
    all_df <- all_df[grepl(type, all_df$fact_name, ignore.case = TRUE),] 
  }
  
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
      locked = grepl("LOCKED", resn, fixed = TRUE),
      confidential = grepl("CONFIDENTIAL", resn, fixed = TRUE),
      private = grepl("PRIVACY", resn, fixed = TRUE)
    )
  }
  
  fact
}

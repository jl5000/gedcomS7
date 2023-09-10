#' @include cls_validators.R
NULL


#' Create a fact object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM EVENT_DETAIL (plus a bit more).
#' @include cls_date.R cls_place.R cls_address.R cls_association.R 
#' cls_note.R cls_citation.R cls_media_link.R
#' @tests
#' expect_error(class_fact("FACT"), regexp = "@fact_desc has too few elements")
#' expect_error(class_fact("BIRT", unique_ids = "ABC"), regexp = "@unique_ids is in an invalid format")
#' expect_snapshot_value(class_fact("BIRT", fact_val = "Y")@.fact_detail_as_ged, "json2")
#' expect_snapshot_value(class_fact("FACT", "Diabetes",
#'                                  fact_desc = "Medical condition",
#'                                  date = "26 JUN 2001",
#'                                  place = class_place("here",
#'                                                      notes = "place note"),
#'                                  address = "street, town, city, country",
#'                                  phone_numbers = "123455",
#'                                  emails = "things@domain.com",
#'                                  web_pages = "www.domain.com",
#'                                  cause = "Chocolate",
#'                                  locked = TRUE,
#'                                  date_sort = "2008",
#'                                  associations = class_association("@I45@", relation_is = "GODP"),
#'                                  notes = "another note",
#'                                  note_xrefs = "@N45@",
#'                                  citations = "@S67@",
#'                                  unique_ids = "7ddf39aa-42a8-4995-94eb-4392bcc00d28")@.fact_detail_as_ged, 
#'                        "json2")
class_fact <- S7::new_class(
  "class_fact",
  properties = list(
    # Not part of detail, but want them to appear first
    fact_type = S7::class_character,
    fact_val = S7::class_character,
    fact_desc = S7::class_character,
    
    date = S7::class_character | class_date_value,
    place = S7::class_character | class_place,
    address = S7::class_character | class_address,
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
    date_sort = S7::class_character | class_date_sort,
    associations = S7::class_list | class_association,
    note_xrefs = S7::class_character,
    notes = S7::class_list | class_note | S7::class_character,
    citations = S7::class_list | class_citation | S7::class_character,
    media_links = S7::class_list | class_media_link | S7::class_character,
    unique_ids = S7::class_character,
    
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
        if(length(self@place) == 1){
          obj_to_val(self@place)
        } else if(length(self@address) == 1) {
          obj_to_val(self@address)
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
          sprintf("0 %s %s", self@fact_type, chronify(self@fact_val)) |> trimws(),
          sprintf("1 TYPE %s", self@fact_desc),
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
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1),
          obj_to_ged(self@media_links, "OBJE") |> increase_level(by = 1),
          sprintf("1 UID %s", self@unique_ids)
        )
      }
    )
  ),
  validator = function(self) {
    c(
      chk_input_size(self@fact_type, "@fact_type", 1, 1),
      # fact_type enum and fact_val checked later
      chk_input_size(self@fact_desc, "@fact_desc", as.integer(self@fact_type %in% c("FACT","EVEN","IDNO")), 1, 1),
      chk_input_size(self@date, "@date", 0, 1),
      chk_input_pattern(self@date, "@date", reg_date_value()),
      chk_input_size(self@place, "@place", 0, 1, 1),
      chk_input_size(self@address, "@address", 0, 1, 1),
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
      chk_input_size(self@date_sort, "@date_sort", 0, 1),
      chk_input_pattern(self@date_sort, "@date_sort", reg_date_gregorian()),
      chk_input_S7classes(self@associations, "@associations", class_association),
      chk_input_pattern(self@note_xrefs, "@note_xrefs", reg_xref(TRUE)),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+"),
      chk_input_S7classes(self@citations, "@citations", class_citation, reg_xref(TRUE)),
      chk_input_S7classes(self@media_links, "@media_links", class_media_link, reg_xref(TRUE)),
      chk_input_pattern(self@unique_ids, "@unique_ids", reg_uuid(TRUE))
    )
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
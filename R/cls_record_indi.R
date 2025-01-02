
#' Create an individual record object
#' 
#' @inheritParams prop_definitions 
#' @param pers_names TODO
#' @param sex The sex of the individual. Either "M" (male), "F" (female), "X" (other), or
#' "U" (undetermined, the default).
#' @param facts Events and/or attributes for this individual.
#' An `IndividualEvent()`/`IndividualAttribute()` object, or a list of them.
#' @param non_events Events that this individual did not experience.
#' A `NonEvent()` object, or a list of them.
#' @param ordinances An `Ordinance()` object, or a list of them.
#' @param fam_links_chil TODO
#' @param fam_links_spou TODO
#' @param alia_xrefs A named character vector of relevant individual record cross-reference identifiers
#' whose records also represent this individual. The vector names may provide a description of these records.
#' @param anci_xrefs A character vector of relevant submitter record cross-reference identifiers
#' who are interested in the ancestors of this individual.
#' @param desi_xrefs A character vector of relevant submitter record cross-reference identifiers
#' who are interested in the descendants of this individual.
#' 
#' @returns An S7 object representing a GEDCOM INDIVIDUAL_RECORD.
#' @export
#' @tests
#' nms <- list(PersonalName("Joe /Bloggs/"),
#'             PersonalName("Joseph /Bloggs/"))
#' fcts <- list(IndividualEvent("BIRT", date = "2005", place = "USA"),
#'              IndividualEvent("BIRT", date = "2006", place = "Colorado, USA"),
#'              IndividualEvent("DEAT", date = "18 JUN 2020", place = "London, UK"),
#'              IndividualEvent("DEAT", date = "2021", place = "UK"))
#' expect_equal(IndividualRecord(pers_names = nms)@c_primary_name, "Joe Bloggs")
#' expect_equal(IndividualRecord(pers_names = nms)@c_all_names, c("Joe Bloggs","Joseph Bloggs"))
#' birt_deat <- IndividualRecord(facts = fcts)
#' expect_equal(birt_deat@c_birth_date, "2005")
#' expect_equal(birt_deat@c_birth_place, "USA")
#' expect_equal(birt_deat@c_death_date, "18 JUN 2020")
#' expect_equal(birt_deat@c_death_place, "London, UK")
#' expect_snapshot_value(IndividualRecord("@I4@", sex = "M", facts = fcts, pers_names = nms,
#'                                         fam_links_chil = "@F132@", 
#'                                         fam_links_spou = "@F67@")@c_as_ged, "json2")
IndividualRecord <- S7::new_class(
  "IndividualRecord", 
  parent = Record,
  properties = list(
    pers_names = S7::new_property(S7::class_list | 
                                    S7::new_S3_class("gedcomS7::PersonalName") | 
                                    S7::class_character,
                                  validator = function(value){
                                    chk_input_S7classes(value, PersonalName, ".+")
                                  }),
    sex = S7::new_property(S7::class_character, default = "U",
                           validator = function(value){
                             c(
                               chk_input_size(value, 0, 1),
                               chk_input_choice(value, val_sexes())
                             )
                           }),
    facts = S7::new_property(S7::class_list | 
                               S7::new_S3_class("gedcomS7::IndividualFact"),
                             validator = function(value){
                               chk_input_S7classes(value, IndividualFact)
                             }),
    non_events = S7::new_property(S7::class_list | 
                                    S7::new_S3_class("gedcomS7::NonEvent"),
                                  validator = function(value){
                                    chk_input_S7classes(value, NonEvent)
                                  }),
    ordinances = S7::new_property(S7::class_list | 
                                    S7::new_S3_class("gedcomS7::Ordinance"),
                                  validator = function(value){
                                    chk_input_S7classes(value, Ordinance)
                                  }),
    fam_links_chil = S7::new_property(S7::class_list | 
                                        S7::new_S3_class("gedcomS7::FamilyLinkChild") | 
                                        S7::class_character,
                                      validator = function(value){
                                        chk_input_S7classes(value, FamilyLinkChild, reg_xref(TRUE))
                                      }),
    fam_links_spou = S7::new_property(S7::class_list | 
                                        S7::new_S3_class("gedcomS7::FamilyLinkSpouse") | 
                                        S7::class_character,
                                      validator = function(value){
                                        chk_input_S7classes(value, FamilyLinkSpouse, reg_xref(TRUE))
                                      }),
    subm_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    associations = S7::new_property(S7::class_list | 
                                      S7::new_S3_class("gedcomS7::Association"),
                                    validator = function(value){
                                      chk_input_S7classes(value, Association)
                                    }),
    alia_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    anci_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    desi_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    
    c_primary_name = S7::new_property(
      S7::class_character,
      getter = function(self){
        if(length(self@pers_names) == 0){
          character()
        } else {
          obj_to_val(as.iterable(self@pers_names)[[1]]) |> 
            gsub(pattern = "/", replacement = "")
        }
      }),
    
    c_all_names = S7::new_property(
      S7::class_character,
      getter = function(self){
        vapply(as.iterable(self@pers_names), \(nm){
          obj_to_val(nm)
        }, FUN.VALUE = character(1), USE.NAMES = FALSE) |> 
          gsub(pattern = "/", replacement = "")
      }),
    
    c_birth_date = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in as.iterable(self@facts)){
          if(fact@fact_type == "BIRT") return(fact@c_fact_date)
        }
        character()
      }),
    
    c_birth_place = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in as.iterable(self@facts)){
          if(fact@fact_type == "BIRT") return(fact@c_fact_location)
        }
        character()
      }),
    
    c_death_date = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in as.iterable(self@facts)){
          if(fact@fact_type == "DEAT") return(fact@c_fact_date)
        }
        character()
      }),
    
    c_death_place = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in as.iterable(self@facts)){
          if(fact@fact_type == "DEAT") return(fact@c_fact_location)
        }
        character()
      }),
    
    c_as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s INDI", self@xref),
          sprintf("1 RESN %s", self@c_restrictions),
          obj_to_ged(self@pers_names, "NAME") |> increase_level(by = 1),
          sprintf("1 SEX %s", self@sex),
          obj_to_ged(self@facts) |> increase_level(by = 1),
          obj_to_ged(self@non_events) |> increase_level(by = 1),
          obj_to_ged(self@ordinances) |> increase_level(by = 1),
          obj_to_ged(self@fam_links_chil, "FAMC") |> increase_level(by = 1),
          obj_to_ged(self@fam_links_spou, "FAMS") |> increase_level(by = 1),
          sprintf("1 SUBM %s", self@subm_xrefs),
          obj_to_ged(self@associations) |> increase_level(by = 1),
          named_vec_to_ged(self@alia_xrefs, "ALIA", "PHRASE") |> increase_level(by = 1),
          sprintf("1 ANCI %s", self@anci_xrefs),
          sprintf("1 DESI %s", self@desi_xrefs),
          self@c_ids_as_ged |> increase_level(by = 1),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1),
          obj_to_ged(self@media_links, "OBJE") |> increase_level(by = 1),
          obj_to_ged(self@updated) |> increase_level(by = 1),
          obj_to_ged(self@created) |> increase_level(by = 1)
        )
      })
  )
)


parse_record_indi <- function(rec_lines){
  
  rec <- IndividualRecord(
    xref = parse_line_xref(rec_lines[1]),
    sex = find_ged_values(rec_lines, "SEX"),
    pers_names = parse_personal_names(rec_lines),
    facts = parse_facts_indi(rec_lines),
    non_events = parse_non_events(rec_lines),
    ordinances = parse_ordinances(rec_lines),
    fam_links_chil = parse_family_links(rec_lines, as_spouse = FALSE),
    fam_links_spou = parse_family_links(rec_lines, as_spouse = TRUE),
    subm_xrefs = find_ged_values(rec_lines, "SUBM"),
    associations = parse_associations(rec_lines),
    alia_xrefs = parse_vals_and_types(rec_lines, "ALIA"),
    anci_xrefs = find_ged_values(rec_lines, "ANCI"),
    desi_xrefs = find_ged_values(rec_lines, "DESI")
  )
  
  parse_common_record_elements(rec, rec_lines)
}

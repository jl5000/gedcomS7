
#' Create an individual record object
#' 
#' @inheritParams prop_definitions 
#' @param pers_names A `PersonalName()` object or a list of them, giving the names
#' of this individual. A simple character vector of names can be provided instead,
#' but this is not recommended. 
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
#' expect_warning(IndividualRecord(pers_names = "Me"),
#'                regexp = "Did you forget to enclose the surname in forward slashes")
#' expect_warning(IndividualRecord(pers_names = list(PersonalName("Joe /Bloggs/"), "Me")),
#'                regexp = "Did you forget to enclose the surname in forward slashes")
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
                                  getter = function(self) self@pers_names,
                                  setter = function(self, value){
                                    self@pers_names <- as.S7class_list(value, gedcomS7::PersonalName)
                                    self
                                  },
                                  validator = function(value){
                                    chk_input_S7classes(value, gedcomS7::PersonalName)
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
                             getter = function(self) self@facts,
                             setter = function(self, value){
                               self@facts <- as.S7class_list(value, IndividualFact)
                               self
                             },
                             validator = function(value){
                               chk_input_S7classes(value, IndividualFact)
                             }),
    non_events = S7::new_property(S7::class_list | 
                                    S7::new_S3_class("gedcomS7::NonEvent"),
                                  getter = function(self) self@non_events,
                                  setter = function(self, value){
                                    self@non_events <- as.S7class_list(value, gedcomS7::NonEvent)
                                    self
                                  },
                                  validator = function(value){
                                    chk_input_S7classes(value, gedcomS7::NonEvent)
                                  }),
    ordinances = S7::new_property(S7::class_list | 
                                    S7::new_S3_class("gedcomS7::Ordinance"),
                                  getter = function(self) self@ordinances,
                                  setter = function(self, value){
                                    self@ordinances <- as.S7class_list(value, gedcomS7::Ordinance)
                                    self
                                  },
                                  validator = function(value){
                                    chk_input_S7classes(value, gedcomS7::Ordinance)
                                  }),
    fam_links_chil = S7::new_property(S7::class_list | 
                                        S7::new_S3_class("gedcomS7::FamilyLinkChild") | 
                                        S7::class_character,
                                      getter = function(self) self@fam_links_chil,
                                      setter = function(self, value){
                                        self@fam_links_chil <- as.S7class_list(value, gedcomS7::FamilyLinkChild)
                                        self
                                      },
                                      validator = function(value){
                                        chk_input_S7classes(value, gedcomS7::FamilyLinkChild)
                                      }),
    fam_links_spou = S7::new_property(S7::class_list | 
                                        S7::new_S3_class("gedcomS7::FamilyLinkSpouse") | 
                                        S7::class_character,
                                      getter = function(self) self@fam_links_spou,
                                      setter = function(self, value){
                                        self@fam_links_spou <- as.S7class_list(value, gedcomS7::FamilyLinkSpouse)
                                        self
                                      },
                                      validator = function(value){
                                        chk_input_S7classes(value, gedcomS7::FamilyLinkSpouse)
                                      }),
    subm_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    associations = S7::new_property(S7::class_list | 
                                      S7::new_S3_class("gedcomS7::Association"),
                                    getter = function(self) self@associations,
                                    setter = function(self, value){
                                      self@associations <- as.S7class_list(value, gedcomS7::Association)
                                      self
                                    },
                                    validator = function(value){
                                      chk_input_S7classes(value, gedcomS7::Association)
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
        if(length(self@pers_names) == 0) return(character())
        
        obj_to_val(self@pers_names[[1]]) |> 
          gsub(pattern = "/", replacement = "")
      }),
    
    c_all_names = S7::new_property(
      S7::class_character,
      getter = function(self){
        vapply(self@pers_names, \(nm){
          obj_to_val(nm)
        }, FUN.VALUE = character(1), USE.NAMES = FALSE) |> 
          gsub(pattern = "/", replacement = "")
      }),
    
    c_birth_date = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact_type == "BIRT") return(fact@c_fact_date)
        }
        character()
      }),
    
    c_birth_place = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact_type == "BIRT") return(fact@c_fact_location)
        }
        character()
      }),
    
    c_death_date = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact_type == "DEAT") return(fact@c_fact_date)
        }
        character()
      }),
    
    c_death_place = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
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

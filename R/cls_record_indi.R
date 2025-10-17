
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
#' @param fam_links_chil A `FamilyLinkChild()` object or a list of them, giving the
#' Family records that this individual is a member of as a child. A character vector
#' of Family record xrefs can also be provided. This will be automatically updated if
#' the individual's membership in a Family record changes.
#' @param fam_links_spou A `FamilyLinkSpouse()` object or a list of them, giving the
#' Family records that this individual is a member of as a spouse. A character vector
#' of Family record xrefs can also be provided. This will be automatically updated if
#' the individual's membership in a Family record changes.
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
#' expect_equal(IndividualRecord(pers_names = nms)@PRIMARY_NAME, "Joe Bloggs")
#' expect_equal(IndividualRecord(pers_names = nms)@ALL_NAMES, c("Joe Bloggs","Joseph Bloggs"))
#' birt_deat <- IndividualRecord(facts = fcts)
#' expect_equal(birt_deat@BIRTH_DATE, "2005")
#' expect_equal(birt_deat@BIRTH_PLACE, "USA")
#' expect_equal(birt_deat@DEATH_DATE, "18 JUN 2020")
#' expect_equal(birt_deat@DEATH_PLACE, "London, UK")
#' expect_snapshot_value(IndividualRecord("@I4@", sex = "M", facts = fcts, pers_names = nms,
#'                                         fam_links_chil = "@F132@", 
#'                                         fam_links_spou = "@F67@")@GEDCOM, "json2")
IndividualRecord <- S7::new_class(
  "IndividualRecord", 
  parent = Record,
  properties = list(
    pers_names = prop_S7list("pers_names", PersonalName),
    sex = prop_char(0, 1, choices = val_sexes(), default = "U"),
    facts = prop_S7list("facts", IndividualFact),
    non_events = prop_S7list("non_events", NonEvent),
    ordinances = prop_S7list("ordinances", Ordinance),
    fam_links_chil = prop_S7list("fam_links_chil", FamilyLinkChild),
    fam_links_spou = prop_S7list("fam_links_spou", FamilyLinkSpouse),
    subm_xrefs = prop_char(pattern = reg_xref(TRUE)),
    associations = prop_S7list("associations", Association),
    alia_xrefs = prop_char(pattern = reg_xref(TRUE)),
    anci_xrefs = prop_char(pattern = reg_xref(TRUE)),
    desi_xrefs = prop_char(pattern = reg_xref(TRUE)),
    
    PRIMARY_NAME = S7::new_property(
      S7::class_character,
      getter = function(self){
        if(length(self@pers_names) == 0) return(character())
        
        as_val(self@pers_names[[1]]) |> 
          gsub(pattern = "/", replacement = "")
      }),
    
    ALL_NAMES = S7::new_property(
      S7::class_character,
      getter = function(self){
        vapply(self@pers_names, \(nm){
          as_val(nm)
        }, FUN.VALUE = character(1), USE.NAMES = FALSE) |> 
          gsub(pattern = "/", replacement = "")
      }),
    
    BIRTH_DATE = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact_type == "BIRT") return(fact@FACT_DATE)
        }
        character()
      }),
    
    BIRTH_PLACE = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact_type == "BIRT") return(fact@FACT_LOCATION)
        }
        character()
      }),
    
    IS_ALIVE = S7::new_property(
      S7::class_logical,
      getter = function(self) is_alive(self@GEDCOM)
    ),
    
    DEATH_DATE = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact_type == "DEAT") return(fact@FACT_DATE)
        }
        character()
      }),
    
    DEATH_PLACE = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact_type == "DEAT") return(fact@FACT_LOCATION)
        }
        character()
      }),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s INDI", self@XREF),
          sprintf("1 RESN %s", restrictions_to_resn(self@confidential, self@locked, self@private)),
          as_ged(self@pers_names) |> level_up(1),
          sprintf("1 SEX %s", self@sex),
          as_ged(self@facts) |> level_up(1),
          as_ged(self@non_events) |> level_up(1),
          as_ged(self@ordinances) |> level_up(1),
          as_ged(self@fam_links_chil) |> level_up(1),
          as_ged(self@fam_links_spou) |> level_up(1),
          sprintf("1 SUBM %s", self@subm_xrefs),
          as_ged(self@associations) |> level_up(1),
          as_ged(self@alia_xrefs, "ALIA", "PHRASE") |> level_up(1),
          sprintf("1 ANCI %s", self@anci_xrefs),
          sprintf("1 DESI %s", self@desi_xrefs),
          identifiers_to_ged(self@user_ids, self@unique_ids, self@ext_ids) |> level_up(1),
          notes_to_ged(self@notes, self@note_xrefs) |> level_up(1),
          as_ged(self@citations) |> level_up(1),
          as_ged(self@media_links) |> level_up(1),
          as_ged(self@updated) |> level_up(1),
          as_ged(self@created) |> level_up(1)
        )
      })
  )
)


parse_record_indi <- function(rec_lines){
  
  rec <- IndividualRecord(
    XREF = parse_line_xref(rec_lines[1]),
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


S7::method(summary, IndividualRecord) <- function(object, ...){
  exdent <- 15
  to_console("XREF:", object@XREF, exdent)
  to_console("Names:", toString(object@ALL_NAMES), exdent)
  to_console("Sex:", object@sex, exdent)
  to_console("Birth Date:", object@BIRTH_DATE, exdent)
  to_console("Birth Place:", object@BIRTH_PLACE, exdent)
  to_console("Alive:", object@IS_ALIVE, exdent)
  if(!object@IS_ALIVE){
    to_console("Death Date:", object@DEATH_DATE, exdent)
    to_console("Death Place:", object@DEATH_PLACE, exdent)
  }
  to_console("Facts:", length(object@facts), exdent)
  cat("\n")
  to_console("Family Links:", sprintf("%s (as child), %s (as spouse)",
                                      length(object@fam_links_chil), length(object@fam_links_spou)), 
             exdent)
  to_console("Indiv. Links:", sprintf("%s (associations), %s (aliases)",
                                      length(object@associations), length(object@alia_xrefs)), 
             exdent)
  to_console("Citations:", length(object@citations), exdent)
  to_console("Media Links:", length(object@media_links), exdent)
  to_console("Notes:", length(object@notes) + length(object@note_xrefs), exdent)
  cat("\n")
  print_record_summary(object)
}

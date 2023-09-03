#' @include cls_validators.R
NULL

#' Create an individual fact object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM INDIVIDUAL_EVENT_DETAIL (plus a bit more).
#' @include cls_fact.R
#' @tests
#' expect_error(class_fact_indi("DEAT", age = "73"), regexp = "@age is in an invalid format")
#' expect_snapshot_value(class_fact_indi("DEAT", fact_val = "Y")@.indi_fact_detail_as_ged, "json2")
#' expect_snapshot_value(class_fact_indi("DEAT", fact_val = "Y", age_phrase = "old")@.indi_fact_detail_as_ged, "json2")
#' expect_snapshot_value(class_fact_indi("DEAT", fact_val = "Y", age = "73y 4m",
#'                                       age_phrase = "old")@.indi_fact_detail_as_ged, "json2")
class_fact_indi <- S7::new_class(
  "class_fact_indi", 
  parent = class_fact,
  properties = list(
    age = S7::class_character,
    age_phrase = S7::class_character,
    
    .indi_fact_detail_as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        age <- self@age
        if(length(self@age_phrase) == 1)
          age <- chronify(self@age)
        
        c(
          self@.fact_detail_as_ged,
          sprintf("1 AGE %s", age) |> trimws(),
          sprintf("2 PHRASE %s", self@age_phrase)
        )
      }
    )
  ),
  validator = function(self){
    c(
      chk_input_size(self@age, "@age", 0, 1),
      chk_input_pattern(self@age, "@age", reg_age_at_event()),
      chk_input_size(self@age_phrase, "@age_phrase", 0, 1, 1)
    )
  }
)


#' Create an individual event object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM INDIVIDUAL_EVENT_STRUCTURE.
#' @export
#' @tests
#' expect_error(class_event_indi("birth", fact_val = "Y"), 
#'              regexp = "@fact_type has an invalid value")
#' expect_error(class_event_indi("BIRT", fact_val = "Yes"), 
#'              regexp = "@fact_val has an invalid value")
#' expect_error(class_event_indi("DEAT", fact_val = "Y", fam_xref = "@12@"), 
#'              regexp = "@fam_xref has too many elements")
#' expect_error(class_event_indi("BIRT", fact_val = "Y", fam_xref = "@12@", adop_parent = "HUSB"), 
#'              regexp = "@adop_parent has too many elements")
#' expect_error(class_event_indi("EVEN", fact_desc = "Fact desc"), 
#'              regexp = "@fact_val has too few elements")
#' expect_error(class_event_indi("ADOP", fact_val = "Y", fam_xref = "@12@", adop_parent = "man"), 
#'              regexp = "@adop_parent has an invalid value")
#' expect_error(class_event_indi("ADOP", fact_val = "Y", adop_parent = "BOTH"), 
#'              regexp = "@adop_parent requires a @fam_xref")
#' expect_error(class_event_indi("ADOP", fact_val = "Y", fam_xref = "@12@", adop_parent_phrase = "both of them"), 
#'              regexp = "@adop_parent_phrase requires a @adop_parent")
#' expect_snapshot_value(class_event_indi("ADOP", fact_val = "Y",
#'                                        fact_desc = "More info on adoption",
#'                                        fam_xref = "@123@",
#'                                        adop_parent = "WIFE",
#'                                        adop_parent_phrase = "Gloria")@as_ged, "json2")
class_event_indi <- S7::new_class(
  "class_event_indi",
  package = "gedcomS7",
  parent = class_fact_indi,
  properties = list(
    fam_xref = S7::class_character,
    adop_parent = S7::class_character,
    adop_parent_phrase = S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          self@.indi_fact_detail_as_ged,
          sprintf("1 FAMC %s", self@fam_xref),
          sprintf("2 ADOP %s", self@adop_parent),
          sprintf("3 PHRASE %s", self@adop_parent_phrase)
        )
      })
  ),
  validator = function(self){
    # Non EVEN events can only have Y value
    fact_val_err <- NULL
    if(self@fact_type != "EVEN")
      fact_val_err <- chk_input_choice(self@fact_val, "@fact_val", "Y")
    
    # Only ADOP, BIRT and CHR can have xref
    fam_xref_err <- NULL
    if(!self@fact_type %in% c("ADOP","BIRT","CHR"))
      fam_xref_err <- chk_input_size(self@fam_xref, "@fam_xref", 0, 0)
    
    # Only ADOP can have xref, adop_parent, and phrase
    adop_err <- NULL
    if(self@fact_type != "ADOP")
      adop_err <- c(
        chk_input_size(self@adop_parent, "@adop_parent", 0, 0),
        chk_input_size(self@adop_parent_phrase, "@adop_parent_phrase", 0, 0)
      )
    
    c(
      chk_input_choice(self@fact_type, "@fact_type", val_individual_event_types(TRUE)),
      chk_input_size(self@fact_val, "@fact_val", as.integer(self@fact_type == "EVEN"), 1, 1),
      fact_val_err,
      fam_xref_err,
      chk_input_size(self@fam_xref, "@fam_xref", 0, 1),
      chk_input_pattern(self@fam_xref, "@fam_xref", reg_xref(TRUE)),
      adop_err,
      chk_input_size(self@adop_parent, "@adop_parent", 0, 1),
      chk_input_choice(self@adop_parent, "@adop_parent", val_adoptive_parents()),
      chk_input_parents(self@adop_parent, "@adop_parent", self@fam_xref, "@fam_xref"),
      chk_input_size(self@adop_parent_phrase, "@adop_parent_phrase", 0, 1, 1),
      chk_input_parents(self@adop_parent_phrase, "@adop_parent_phrase", self@adop_parent, "@adop_parent")
    )
  }
)


#' Create an individual attribute object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM INDIVIDUAL_ATTRIBUTE_STRUCTURE.
#' @export
#' @tests
#' expect_error(class_attr_indi("descr", fact_val = "Tall"), 
#'              regexp = "@fact_type has an invalid value")
#' expect_error(class_attr_indi("DSCR"), 
#'              regexp = "@fact_val has too few elements")
#' expect_error(class_attr_indi("NCHI", fact_val = "2.4"), 
#'              regexp = "@fact_val is in an invalid format")
class_attr_indi <- S7::new_class(
  "class_attr_indi",
  package = "gedcomS7",
  parent = class_fact_indi,
  properties = list(
    as_ged = S7::new_property(S7::class_character, 
                              getter = function(self) self@.indi_fact_detail_as_ged)
  ),
  validator = function(self){
    integer_err <- NULL
    if(self@fact_type %in% c("NCHI","NMR"))
      integer_err <- chk_input_pattern(self@fact_val, "@fact_val", "^\\d+$")
      
    c(
      chk_input_choice(self@fact_type, "@fact_type", val_individual_attribute_types(TRUE)),
      chk_input_size(self@fact_val, "@fact_val", 1, 1, 1),
      integer_err
    )
  }
)


extract_facts_indi <- function(rec_lines){
  fact_lst <- find_ged_values(rec_lines, return_list = TRUE,
                              tag = paste(c(val_attribute_types(),
                                            val_individual_event_types()),
                                          collapse = "|"))
  if(length(fact_lst) == 0) return(list())
  
  lapply(fact_lst, \(x){
    tag <- extract_ged_tag(x[1])
    
    nts <- find_ged_values(x, c(tag, "NOTE"))
    fact_date <- find_ged_values(x, c(tag, "DATE"))
    if(length(fact_date) == 1 && !grepl(reg_custom_value(), fact_date)){
      fact_date <- toupper(fact_date)
      fact_date <- sub("@#DGREGORIAN@ ", "", fact_date)
    }
    
    class_fact_indi(
      fact = tag,
      description = find_ged_values(x, tag),
      age = find_ged_values(x, c(tag, "AGE")),
      famg_xref = find_ged_values(x, c(tag, "FAMC")),
      adopting_parent = toupper(find_ged_values(x, c(tag, "FAMC","ADOP"))),
      type = find_ged_values(x, c(tag, "TYPE")),
      date = fact_date,
      place = extract_place(x, tag),
      address = extract_address(x, tag),
      agency = find_ged_values(x, c(tag, "AGNC")),
      relig_affil = find_ged_values(x, c(tag, "RELI")),
      cause = find_ged_values(x, c(tag, "CAUS")),
      note_links = nts[grepl(reg_xref(TRUE), nts)],
      notes = nts[!grepl(reg_xref(TRUE), nts)],
      citations = extract_citations(x, tag),
      media_links = find_ged_values(x, c(tag, "OBJE"))
    )
  })
}

# DOESN'T WORK
# class_indi_birth <- S7::new_class("class_indi_birth", package = "gedcomS7", parent = class_event_indi,
#   properties = list(fact_type = S7::new_property(S7::class_character, getter = function(self) "BIRT"))
# )
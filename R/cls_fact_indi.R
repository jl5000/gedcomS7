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
#' 
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


#' @export
#' @tests
#' 
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
      integer_err
    )
  }
)

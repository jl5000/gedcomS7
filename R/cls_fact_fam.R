#' @include cls_validators.R
NULL

#' Create a family fact object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM FAMILY_EVENT_DETAIL (plus a bit more).
#' @include cls_fact.R
class_fact_fam <- S7::new_class(
  "class_fact_fam", 
  parent = class_fact,
  abstract = TRUE,
  properties = list(
    husb_age = S7::class_character,
    husb_age_phrase = S7::class_character,
    wife_age = S7::class_character,
    wife_age_phrase = S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        husb_age <- self@husb_age
        if(length(self@husb_age_phrase) == 1)
          husb_age <- chronify(self@husb_age)
        
        wife_age <- self@wife_age
        if(length(self@wife_age_phrase) == 1)
          wife_age <- chronify(self@wife_age)
        
        c(
          self@.fact_detail_as_ged,
          rep("1 HUSB", length(husb_age)),
          sprintf("2 AGE %s", husb_age) |> trimws(),
          sprintf("3 PHRASE %s", self@husb_age_phrase),
          rep("1 WIFE", length(wife_age)),
          sprintf("2 AGE %s", wife_age) |> trimws(),
          sprintf("3 PHRASE %s", self@wife_age_phrase)
        )
      }
    )
  ),
  validator = function(self){
    c(
      chk_input_size(self@husb_age, "@husb_age", 0, 1),
      chk_input_pattern(self@husb_age, "@husb_age", reg_age_at_event()),
      chk_input_size(self@husb_age_phrase, "@husb_age_phrase", 0, 1, 1),
      chk_input_size(self@wife_age, "@wife_age", 0, 1),
      chk_input_pattern(self@wife_age, "@wife_age", reg_age_at_event()),
      chk_input_size(self@wife_age_phrase, "@wife_age_phrase", 0, 1, 1)
    )
  }
)


#' Create a family event object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM FAMILY_EVENT_STRUCTURE.
#' @export
#' @tests
#' expect_error(class_event_fam("marr", fact_val = "Y"), 
#'              regexp = "@fact_type has an invalid value")
#' expect_error(class_event_fam("MARR", fact_val = "Yes"), 
#'              regexp = "@fact_val has an invalid value")
#' expect_error(class_event_fam("EVEN", fact_desc = "Fact desc"), 
#'              regexp = "@fact_val has too few elements")       
#' expect_error(class_event_fam("DIV", fact_val = "Y", husb_age = "73"), regexp = "@husb_age is in an invalid format")
#' expect_snapshot_value(class_event_fam("DIV", fact_val = "Y")@as_ged, "json2")
#' expect_snapshot_value(class_event_fam("DIV", fact_val = "Y", wife_age_phrase = "old")@as_ged, "json2")
#' expect_snapshot_value(class_event_fam("DIV", fact_val = "Y", husb_age = "73y 4m",
#'                                       wife_age = "60y")@as_ged, "json2")    
class_event_fam <- S7::new_class(
  "class_event_fam",
  package = "gedcomS7",
  parent = class_fact_fam,
  validator = function(self){
    # Non EVEN events can only have Y value
    fact_val_err <- NULL
    if(self@fact_type != "EVEN")
      fact_val_err <- chk_input_choice(self@fact_val, "@fact_val", "Y")
    
    c(
      chk_input_choice(self@fact_type, "@fact_type", val_family_event_types(TRUE)),
      chk_input_size(self@fact_val, "@fact_val", as.integer(self@fact_type == "EVEN"), 1, 1),
      fact_val_err
    )
  }
)

#' Create a family attribute object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM FAMILY_ATTRIBUTE_STRUCTURE.
#' @export
#' @tests
#' expect_error(class_attr_fam("residence", fact_val = "Earth"), 
#'              regexp = "@fact_type has an invalid value")
#' expect_error(class_attr_fam("RESI", fact_val = ""), 
#'              regexp = "@fact_val has too few characters")   
#' expect_error(class_attr_fam("FACT"), regexp = "@fact_desc has too few elements")   

class_attr_fam <- S7::new_class(
  "class_attr_fam",
  package = "gedcomS7",
  parent = class_fact_fam,
  validator = function(self){
    c(
      chk_input_choice(self@fact_type, "@fact_type", val_family_attribute_types(TRUE)),
      chk_input_size(self@fact_val, "@fact_val", self@fact_type != "RESI", 1, 1)
    )
  }
)

extract_facts_fam <- function(rec_lines){
  fact_lst <- find_ged_values(rec_lines, return_list = TRUE,
                              tag = paste(c(val_family_attribute_types(TRUE),
                                            val_family_event_types(TRUE)),
                                          collapse = "|"))
  if(length(fact_lst) == 0) return(list())
  
  lapply(fact_lst, \(x){
    tag <- extract_ged_tag(x[1])
    
    if(tag %in% val_family_attribute_types(TRUE)){
      fact <- class_attr_fam(
        fact_type = tag,
        fact_val = find_ged_values(x, tag),
        fact_desc = find_ged_values(x, c(tag, "TYPE"))
      )
    } else {
      fact <- class_event_fam(
        fact_type = tag,
        fact_val = find_ged_values(x, tag),
        fact_desc = find_ged_values(x, c(tag, "TYPE"))
      )
    }
    
    S7::props(fact) <- list(
      husb_age = find_ged_values(x, c(tag, "HUSB", "AGE")),
      husb_age_phrase = find_ged_values(x, c(tag, "HUSB", "AGE", "PHRASE")),
      wife_age = find_ged_values(x, c(tag, "WIFE", "AGE")),
      wife_age_phrase = find_ged_values(x, c(tag, "WIFE", "AGE", "PHRASE"))
    )
    
    extract_common_fact_elements(fact, x)
  })
}

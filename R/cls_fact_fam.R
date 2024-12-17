
#' Create a family fact object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM FAMILY_EVENT_DETAIL (plus a bit more).
class_fact_fam <- S7::new_class(
  "class_fact_fam", 
  parent = class_fact,
  abstract = TRUE,
  properties = list(
    husb_age = S7::new_property(S7::class_character,
                                validator = function(value){
                                  c(
                                    chk_input_size(value, 0, 1),
                                    chk_input_pattern(value, reg_age_at_event())
                                  )
                                }),
    husb_age_phrase = S7::new_property(S7::class_character,
                                       validator = function(value){
                                         chk_input_size(value, 0, 1, 1)
                                       }),
    wife_age = S7::new_property(S7::class_character,
                                validator = function(value){
                                  c(
                                    chk_input_size(value, 0, 1),
                                    chk_input_pattern(value, reg_age_at_event())
                                  )
                                }),
    wife_age_phrase = S7::new_property(S7::class_character,
                                       validator = function(value){
                                         chk_input_size(value, 0, 1, 1)
                                       }),
    
    c_as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        husb_age <- self@husb_age
        if(length(self@husb_age_phrase) == 1)
          husb_age <- chronify(self@husb_age)
        
        wife_age <- self@wife_age
        if(length(self@wife_age_phrase) == 1)
          wife_age <- chronify(self@wife_age)
        
        c(
          sprintf("0 %s %s", self@fact_type, chronify(self@fact_val)) |> trimws(),
          sprintf("1 TYPE %s", self@fact_desc),
          rep("1 HUSB", length(husb_age)),
          sprintf("2 AGE %s", husb_age) |> trimws(),
          sprintf("3 PHRASE %s", self@husb_age_phrase),
          rep("1 WIFE", length(wife_age)),
          sprintf("2 AGE %s", wife_age) |> trimws(),
          sprintf("3 PHRASE %s", self@wife_age_phrase),
          self@.fact_detail_as_ged
        )
      }
    )
  )
)


#' Create a family event object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM FAMILY_EVENT_STRUCTURE.
#' @export
#' @tests
#' expect_error(class_event_fam("marr", fact_val = "Y"), 
#'              regexp = "This is not a valid @fact_type for this event")
#' expect_error(class_event_fam("MARR", fact_val = "Yes"), 
#'              regexp = "Only a @fact_val of 'Y' is permitted for this event")
#' expect_error(class_event_fam("EVEN", fact_desc = "Fact desc"), 
#'              regexp = "A @fact_val is required for this fact")       
#' expect_error(class_event_fam("DIV", fact_val = "Y", husb_age = "73"), regexp = "@husb_age is in an invalid format")
#' expect_snapshot_value(class_event_fam("DIV", fact_val = "Y")@c_as_ged, "json2")
#' expect_snapshot_value(class_event_fam("DIV", fact_val = "Y", wife_age_phrase = "old")@c_as_ged, "json2")
#' expect_snapshot_value(class_event_fam("DIV", fact_val = "Y", husb_age = "73y 4m",
#'                                       wife_age = "60y")@c_as_ged, "json2")    
class_event_fam <- S7::new_class(
  "class_event_fam",
  package = "gedcomS7",
  parent = class_fact_fam,
  validator = function(self){
    if(!self@fact_type %in% val_family_event_types(TRUE))
      return("This is not a valid @fact_type for this event.")
  }
)

#' Create a family attribute object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM FAMILY_ATTRIBUTE_STRUCTURE.
#' @export
#' @tests
#' expect_error(class_attr_fam("residence", fact_val = "Earth"), 
#'              regexp = "This is not a valid @fact_type for this attribute")
#' expect_error(class_attr_fam("RESI", fact_val = ""), 
#'              regexp = "@fact_val has too few characters")   
#' expect_error(class_attr_fam("FACT"), regexp = "A @fact_val is required for this fact")   
class_attr_fam <- S7::new_class(
  "class_attr_fam",
  package = "gedcomS7",
  parent = class_fact_fam,
  validator = function(self){
    if(!self@fact_type %in% val_family_attribute_types(TRUE))
      return("This is not a valid @fact_type for this attribute.")
  }
)

parse_facts_fam <- function(rec_lines){
  fact_lst <- find_ged_values(rec_lines, return_list = TRUE,
                              tag = paste(c(val_family_attribute_types(TRUE),
                                            val_family_event_types(TRUE)),
                                          collapse = "|"))
  if(length(fact_lst) == 0) return(list())
  
  lapply(fact_lst, \(x){
    tag <- parse_line_tag(x[1])
    
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
    
    parse_common_fact_elements(fact, x)
  })
}


FamilyFact <- S7::new_class(
  "FamilyFact", 
  parent = Fact,
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
    
    GEDCOM = S7::new_property(
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
#' @returns An S7 object representing a GEDCOM FAMILY_EVENT_STRUCTURE.
#' @export
#' @tests
#' expect_error(FamilyEvent("marr", fact_val = "Y"), 
#'              regexp = "This is not a valid @fact_type for this event")
#' expect_error(FamilyEvent("MARR", fact_val = "Yes"), 
#'              regexp = "Only a @fact_val of 'Y' is permitted for this event")
#' expect_error(FamilyEvent("EVEN", fact_desc = "Fact desc"), 
#'              regexp = "A @fact_val is required for this fact")       
#' expect_error(FamilyEvent("DIV", fact_val = "Y", husb_age = "73"), regexp = "@husb_age is in an invalid format")
#' expect_snapshot_value(FamilyEvent("DIV", fact_val = "Y")@GEDCOM, "json2")
#' expect_snapshot_value(FamilyEvent("DIV", fact_val = "Y", wife_age_phrase = "old")@GEDCOM, "json2")
#' expect_snapshot_value(FamilyEvent("DIV", fact_val = "Y", husb_age = "73y 4m",
#'                                       wife_age = "60y")@GEDCOM, "json2")    
FamilyEvent <- S7::new_class(
  "FamilyEvent",
  parent = FamilyFact,
  validator = function(self){
    if(!self@fact_type %in% val_family_event_types(TRUE))
      return("This is not a valid @fact_type for this event.")
  }
)

#' Create a family attribute object
#' 
#' @inheritParams prop_definitions 
#' @returns An S7 object representing a GEDCOM FAMILY_ATTRIBUTE_STRUCTURE.
#' @export
#' @tests
#' expect_error(FamilyAttribute("residence", fact_val = "Earth"), 
#'              regexp = "This is not a valid @fact_type for this attribute")
#' expect_error(FamilyAttribute("RESI", fact_val = ""), 
#'              regexp = "@fact_val has too few characters")   
#' expect_error(FamilyAttribute("FACT"), regexp = "A @fact_val is required for this fact")
#' expect_error(FamilyAttribute("NCHI", 3.2), regexp = "Number of children/marriages must be a whole number")
#' expect_error(FamilyAttribute("NCHI", fact_val = "3", date = ""),
#'              regexp = "A blank @date requires a @date_phrase")   
FamilyAttribute <- S7::new_class(
  "FamilyAttribute",
  parent = FamilyFact,
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
      fact <- FamilyAttribute(
        fact_type = tag,
        fact_val = find_ged_values(x, tag),
        fact_desc = find_ged_values(x, c(tag, "TYPE"))
      )
    } else {
      fact <- FamilyEvent(
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

S7::method(summary, FamilyFact) <- function(object, ...){
  exdent <- 15
  fact_type <- object@fact_type
  fact_type <- names(val_fact_types(TRUE))[fact_type == val_fact_types(TRUE)]
  to_console_value_with_phrase("Fact Type:", 
                               fact_type, object@fact_desc, 
                               exdent)
  if(length(object@fact_val) == 1) to_console("Value:", object@fact_val, exdent)
  to_console_value_with_phrase("Husband Age:", 
                               object@husb_age, object@husb_age_phrase, 
                               exdent)
  to_console_value_with_phrase("Wife Age:", 
                               object@wife_age, object@wife_age_phrase, 
                               exdent)
  to_console("Cause:", object@cause, exdent)
  to_console("Date:", object@FACT_DATE, exdent)
  to_console("Location:", object@FACT_LOCATION, exdent)
  to_console_list("Phone Numbers:", object@phone_numbers, exdent)
  to_console_list("Fax Numbers:", object@faxes, exdent)
  to_console_list("Emails:", object@emails, exdent)
  to_console_list("Web Pages:", object@web_pages, exdent)
  cat("\n")
  to_console("Associations:", length(object@associations), exdent)
  to_console("Citations:", length(object@citations), exdent)
  to_console("Media Links:", length(object@media_links), exdent)
  to_console("Notes:", length(object@notes) + length(object@note_xrefs), exdent)
  cat("\n")
  to_console("Unique IDs:", toString(object@unique_ids), exdent)
  to_console("Restrictions:", object@RESTRICTIONS, exdent)
}



IndividualFact <- S7::new_class(
  "IndividualFact", 
  parent = Fact,
  abstract = TRUE,
  properties = list(
    age = prop_char(0, 1, pattern = reg_age_at_event()),
    age_phrase = prop_char(0, 1, 1)
  )
)


#' Create an individual event object
#' 
#' @inheritParams prop_definitions 
#' @param adop_parent The adopting parent. Must be a value from `val_adoptive_parents()`. 
#' For adoption events only.
#' @param adop_parent_phrase A free text description of the adoptive parent (if the
#' value itself is insufficient).
#' 
#' @returns An S7 object representing a GEDCOM INDIVIDUAL_EVENT_STRUCTURE.
#' @export
#' @tests
#' expect_error(IndividualEvent("birth", fact_val = "Y"), 
#'              regexp = "This is not a valid @fact_type for this event")
#' expect_error(IndividualEvent("BIRT", fact_val = "Yes"), 
#'              regexp = "Only a @fact_val of 'Y' is permitted for this event")
#' expect_error(IndividualEvent("DEAT", fact_val = "Y", fam_xref = "@12@"), 
#'              regexp = "Only adoption, birth, and christening events can have a @fam_xref")
#' expect_error(IndividualEvent("BIRT", fact_val = "Y", fam_xref = "@12@", adop_parent = "HUSB"), 
#'              regexp = "Only adoption events can have a @adop_parent or @adop_parent_phrase")
#' expect_error(IndividualEvent("EVEN", fact_desc = "Fact desc"), 
#'              regexp = "A @fact_val is required for this fact")
#' expect_error(IndividualEvent("ADOP", fact_val = "Y", fam_xref = "@12@", adop_parent = "man"), 
#'              regexp = "@adop_parent has an invalid value")
#' expect_error(IndividualEvent("ADOP", fact_val = "Y", adop_parent = "BOTH"), 
#'              regexp = "@adop_parent requires a @fam_xref")
#' expect_error(IndividualEvent("ADOP", fact_val = "Y", fam_xref = "@12@", adop_parent_phrase = "both of them"), 
#'              regexp = "@adop_parent_phrase requires a @adop_parent")
#' expect_error(IndividualEvent("BIRT", unique_ids = "ABC"), regexp = "@unique_ids is in an invalid format")
#' expect_snapshot_value(IndividualEvent("BIRT", fact_val = "Y")@GEDCOM, "json2")
#' expect_error(IndividualEvent("DEAT", age = "73"), regexp = "@age is in an invalid format")
#' expect_snapshot_value(IndividualEvent("DEAT", fact_val = "Y")@GEDCOM, "json2")
#' expect_snapshot_value(IndividualEvent("DEAT", fact_val = "Y", age_phrase = "old")@GEDCOM, "json2")
#' expect_snapshot_value(IndividualEvent("DEAT", fact_val = "Y", age = "73y 4m",
#'                                       age_phrase = "old")@GEDCOM, "json2")
#' expect_snapshot_value(IndividualEvent("ADOP", fact_val = "Y",
#'                                        date = "jan 1980",
#'                                        fact_desc = "More info on adoption",
#'                                        fam_xref = "@123@",
#'                                        adop_parent = "WIFE",
#'                                        adop_parent_phrase = "Gloria")@GEDCOM, "json2")
IndividualEvent <- S7::new_class(
  "IndividualEvent",
  parent = IndividualFact,
  properties = list(
    fam_xref = prop_char(0, 1, pattern = reg_xref(TRUE)),
    adop_parent = prop_char(0, 1, choices = val_adoptive_parents()),
    adop_parent_phrase = prop_char(0, 1, 1),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        age <- self@age
        if(length(self@age_phrase) == 1)
          age <- chronify(self@age)
        
        c(
          sprintf("0 %s %s", self@fact_type, chronify(self@fact_val)) |> trimws(),
          sprintf("1 TYPE %s", self@fact_desc),
          sprintf("1 AGE %s", age) |> trimws(),
          sprintf("2 PHRASE %s", self@age_phrase),
          self@.fact_detail_as_ged,
          sprintf("1 FAMC %s", self@fam_xref),
          sprintf("2 ADOP %s", self@adop_parent),
          sprintf("3 PHRASE %s", self@adop_parent_phrase)
        )
      })
  ),
  validator = function(self){
    errs <- NULL
    if(!self@fact_type %in% val_individual_event_types(TRUE))
      errs <- c(errs, "This is not a valid @fact_type for this event.")
    
    # Only ADOP, BIRT and CHR can have xref
    if(length(self@fam_xref) > 0 && !self@fact_type %in% c("ADOP","BIRT","CHR"))
      errs <- c(errs, "Only adoption, birth, and christening events can have a @fam_xref")
    
    if(length(self@adop_parent) + length(self@adop_parent_phrase) > 0 &&
       self@fact_type != "ADOP")
      errs <- c(errs, "Only adoption events can have a @adop_parent or @adop_parent_phrase")
    
    c(
      errs,
      chk_input_parents(self@adop_parent, "@adop_parent", self@fam_xref, "@fam_xref"),
      chk_input_parents(self@adop_parent_phrase, "@adop_parent_phrase", self@adop_parent, "@adop_parent")
    )
  }
)


#' Create an individual attribute object
#' 
#' @inheritParams prop_definitions 
#' @returns An S7 object representing a GEDCOM INDIVIDUAL_ATTRIBUTE_STRUCTURE.
#' @export
#' @tests
#' expect_error(IndividualAttribute("descr", fact_val = "Tall"), 
#'              regexp = "This is not a valid @fact_type for this attribute")
#' expect_error(IndividualAttribute("DSCR"), 
#'              regexp = "A @fact_val is required for this fact")
#' expect_error(IndividualAttribute("NCHI", fact_val = "2.4"), 
#'              regexp = "Number of children/marriages must be a whole number")
#' expect_snapshot_value(IndividualAttribute("NCHI", 3)@GEDCOM, "json2")
#' expect_snapshot_value(IndividualAttribute("FACT", "Diabetes",
#'                                  fact_desc = "Medical condition",
#'                                  date = "26 JUN 2001",
#'                                  place = Place("here",
#'                                                      notes = "place note"),
#'                                  address = "street, town, city, country",
#'                                  phone_numbers = "123455",
#'                                  emails = "things@domain.com",
#'                                  web_pages = "www.domain.com",
#'                                  cause = "Chocolate",
#'                                  locked = TRUE,
#'                                  date_sort = "2008",
#'                                  associations = Association("@I45@", relation_is = "GODP"),
#'                                  notes = "another note",
#'                                  note_xrefs = "@N45@",
#'                                  citations = "@S67@",
#'                                  unique_ids = "7ddf39aa-42a8-4995-94eb-4392bcc00d28")@GEDCOM, 
#'                        "json2")
IndividualAttribute <- S7::new_class(
  "IndividualAttribute",
  parent = IndividualFact,
  properties = list(
    GEDCOM = S7::new_property(
      S7::class_character, 
      getter = function(self){
        age <- self@age
        if(length(self@age_phrase) == 1)
          age <- chronify(self@age)
        
        c(
          sprintf("0 %s %s", self@fact_type, chronify(self@fact_val)) |> trimws(),
          sprintf("1 TYPE %s", self@fact_desc),
          sprintf("1 AGE %s", age) |> trimws(),
          sprintf("2 PHRASE %s", self@age_phrase),
          self@.fact_detail_as_ged
        )
      })
  ),
  validator = function(self){
    errs <- NULL
    if(!self@fact_type %in% val_individual_attribute_types(TRUE))
      errs <- c(errs, "This is not a valid @fact_type for this attribute.")
      
    errs
  }
)


parse_facts_indi <- function(rec_lines){
  fact_lst <- find_ged_values(rec_lines, return_list = TRUE,
                              tag = paste(c(val_individual_attribute_types(TRUE),
                                            val_individual_event_types(TRUE)),
                                          collapse = "|"))
  if(length(fact_lst) == 0) return(list())
  
  lapply(fact_lst, \(x){
    tag <- parse_line_tag(x[1])
    
    if(tag %in% val_individual_attribute_types(TRUE)){
      fact <- IndividualAttribute(
        fact_type = tag,
        fact_val = find_ged_values(x, tag),
        fact_desc = find_ged_values(x, c(tag, "TYPE"))
      )
    } else {
      fact <- IndividualEvent(
        fact_type = tag,
        fact_val = find_ged_values(x, tag),
        fact_desc = find_ged_values(x, c(tag, "TYPE")),

        fam_xref = find_ged_values(x, c(tag, "FAMC")),
        adop_parent = find_ged_values(x, c(tag, "FAMC","ADOP")),
        adop_parent_phrase = find_ged_values(x, c(tag, "FAMC","ADOP","PHRASE"))
      )
    }
    
    S7::props(fact) <- list(
      age = find_ged_values(x, c(tag, "AGE")),
      age_phrase = find_ged_values(x, c(tag, "AGE", "PHRASE"))
    )
    
    parse_common_fact_elements(fact, x)
  })
}

S7::method(summary, IndividualFact) <- function(object, ...){
  exdent <- 15
  fact_type <- object@fact_type
  fact_type <- names(val_fact_types(TRUE))[fact_type == val_fact_types(TRUE)]
  to_console_value_with_phrase("Fact Type:", 
                               fact_type, object@fact_desc, 
                               exdent)
  if(length(object@fact_val) == 1) to_console("Value:", object@fact_val, exdent)
  to_console_value_with_phrase("Age:", 
                               object@age, object@age_phrase, 
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

# DOESN'T WORK
# class_indi_birth <- S7::new_class("class_indi_birth", parent = IndividualEvent,
#   properties = list(fact_type = S7::new_property(S7::class_character, getter = function(self) "BIRT"))
# )

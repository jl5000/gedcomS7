
#' Create a family record object
#' 
#' @inheritParams prop_definitions 
#' @param facts Events and/or attributes for this family.
#' A `FamilyEvent()`/`FamilyAttribute()` object, or a list of them.
#' @param non_events Events that this family did not experience.
#' A `NonEvent()` object, or a list of them.
#' @param husb_xref,wife_xref,chil_xrefs The cross-reference identifier(s) of member's individual records.
#' If the individual does not have a record, then the value "@VOID@" can be used. 
#' However, you will need to describe the individual by using a named vector (a description can be used
#' in either case), e.g. c("Joe Bloggs" = "@VOID@") or c("Joe Bloggs" = "@I1@").
#' @param spouse_sealings A `SpouseSealing()` object or a list of them detailing 
#' the sealing of a husband and wife in a temple ceremony of The Church of Jesus 
#' Christ of Latter-day Saints.
#' 
#' @returns An S7 object representing a GEDCOM FAMILY_RECORD.
#' @export
#' @tests
#' fct <- list(FamilyEvent("MARR", husb_age = "22y", wife_age = "28y 6m",
#'                            date = "22 AUG 1907", place = "Church"))
#' nevent <- list(NonEvent("DIV"))
#' expect_snapshot_value(FamilyRecord(XREF = "@F2@",
#'                                        facts = fct, non_events = nevent,
#'                                        husb_xref = "@I8@", wife_xref = "@I9@",
#'                                        chil_xrefs = c("@I98@", Eldest = "@I67@"),
#'                                        locked = TRUE,
#'                                        citations = c("@S34@","@S65@"))@GEDCOM, "json2")
#' expect_error(FamilyRecord("REF"), regexp = "@XREF is in an invalid format")
#' expect_error(FamilyRecord(unique_ids = letters), regexp = "@unique_ids is in an invalid format")
#' expect_error(FamilyRecord(ext_ids = LETTERS), regexp = "@ext_ids has too few elements")
#' expect_snapshot_value(FamilyRecord("@1@",
#'                                    unique_ids = "a95b5007-2ad2-4bac-81b0-7184243c4512",
#'                                    ext_ids = stats::setNames(letters, LETTERS)[1:5],
#'                                    user_ids = month.abb[1:6])@GEDCOM, "json2")
#' expect_equal(FamilyRecord(facts = FamilyAttribute("NCHI", 3),
#'                           chil_xrefs = c("@1@","@2@"))@NUM_CHILDREN, 3)
#' expect_equal(FamilyRecord(facts = FamilyAttribute("NCHI", 2),
#'                           chil_xrefs = c("@1@","@2@","@3@"))@NUM_CHILDREN, 3)
FamilyRecord <- S7::new_class(
  "FamilyRecord", 
  parent = Record,
  properties = list(
    facts = prop_S7list("facts", FamilyFact),
    non_events = prop_S7list("non_events", NonEvent),
    husb_xref = prop_char(0, 1, pattern = reg_xref(TRUE)),
    wife_xref = prop_char(0, 1, pattern = reg_xref(TRUE)),
    chil_xrefs = prop_char(pattern = reg_xref(TRUE)),
    associations = prop_S7list("associations", Association),
    subm_xrefs = prop_char(pattern = reg_xref(TRUE)),
    spouse_sealings = prop_S7list("spouse_sealings", SpouseSealing),
    
    NUM_CHILDREN = S7::new_property(
      S7::class_integer,
      getter = function(self){
        nchi <- 0
        for(fact in self@facts){
          if(fact@fact_type == "NCHI"){
            nchi <- as.integer(fact@fact_val)
            break
          }
        }
        max(nchi, length(self@chil_xrefs))
      }),
    
    MARRIAGE_DATE = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact_type == "MARR") return(fact@FACT_DATE)
        }
        character()
      }),
    
    MARRIAGE_PLACE = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact_type == "MARR") return(fact@FACT_LOCATION)
        }
        character()
      }),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          as_ged("FAM", self@XREF),
          restrictions_ged(self@confidential, self@locked, self@private, 1),
          as_ged(self@facts, 1),
          as_ged(self@non_events, 1),
          as_ged(self@husb_xref, c("HUSB", "PHRASE"), 1),
          as_ged(self@wife_xref, c("WIFE", "PHRASE"), 1),
          as_ged(self@chil_xrefs, c("CHIL", "PHRASE"), 1),
          as_ged(self@associations, 1),
          as_ged(self@subm_xrefs, "SUBM", 1),
          as_ged(self@spouse_sealings, 1),
          identifiers_ged(self@user_ids, self@unique_ids, self@ext_ids, 1),
          notes_ged(self@notes, self@note_xrefs, 1),
          as_ged(self@citations, 1),
          as_ged(self@media_links, 1),
          audit_ged(self@updated, self@created, 1)
        )
      })
  )
)


parse_record_fam <- function(rec_lines){
  
  rec <- FamilyRecord(
    XREF = parse_line_xref(rec_lines[1]),
    facts = parse_facts_fam(rec_lines),
    non_events = parse_non_events(rec_lines),
    husb_xref = parse_vals_and_types(rec_lines, "HUSB"),
    wife_xref = parse_vals_and_types(rec_lines, "WIFE"),
    chil_xrefs = parse_vals_and_types(rec_lines, "CHIL"),
    associations = parse_associations(rec_lines),
    subm_xrefs = find_ged_values(rec_lines, "SUBM"),
    spouse_sealings = parse_ordinances(rec_lines)
  )
  
  parse_common_record_elements(rec, rec_lines)
}


S7::method(summary, FamilyRecord) <- function(object, ...){
  exdent <- 15
  to_console("XREF:", object@XREF, exdent)
  to_console("Husband:", object@husb_xref, exdent)
  to_console("Wife:", object@wife_xref, exdent)
  to_console("Children:", object@NUM_CHILDREN, exdent)
  to_console("Marr. Date:", object@MARRIAGE_DATE, exdent)
  to_console("Marr. Place:", object@MARRIAGE_PLACE, exdent)
  to_console("Facts:", length(object@facts), exdent)
  cat("\n")
  to_console("Citations:", length(object@citations), exdent)
  to_console("Media Links:", length(object@media_links), exdent)
  to_console("Notes:", length(object@notes) + length(object@note_xrefs), exdent)
  cat("\n")
  print_record_summary(object)
}

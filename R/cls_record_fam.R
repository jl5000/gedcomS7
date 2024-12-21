
#' Create a family record object
#' 
#' @inheritParams prop_definitions 
#' @param husb_xref,wife_xref,chil_xrefs The cross-reference identifier(s) of member's individual records.
#' If the individual does not have a record, then the value "@VOID@" can be used. 
#' However, you will need to describe the individual by using a named vector (a description can be used
#' in either case), e.g. c("Joe Bloggs" = "@VOID@") or c("Joe Bloggs" = "@I1@").
#' @param spouse_sealings TODO
#' 
#' @return An S7 object representing a GEDCOM FAMILY_RECORD.
#' @export
#' @tests
#' fct <- list(FamilyEvent("MARR", husb_age = "22y", wife_age = "28y 6m",
#'                            date = "22 AUG 1907", place = "Church"))
#' nevent <- list(NonEvent("DIV"))
#' expect_snapshot_value(FamilyRecord(xref = "@F2@",
#'                                        facts = fct, non_events = nevent,
#'                                        husb_xref = "@I8@", wife_xref = "@I9@",
#'                                        chil_xrefs = c("@I98@", Eldest = "@I67@"),
#'                                        locked = TRUE,
#'                                        citations = c("@S34@","@S65@"))@c_as_ged, "json2")
#' expect_error(FamilyRecord("REF"), regexp = "@xref is in an invalid format")
#' expect_error(FamilyRecord("@1@", unique_ids = letters), regexp = "@unique_ids is in an invalid format")
#' expect_error(FamilyRecord("@1@", ext_ids = LETTERS), regexp = "@ext_ids has too few elements")
#' expect_snapshot_value(FamilyRecord("@1@",
#'                                    unique_ids = "a95b5007-2ad2-4bac-81b0-7184243c4512",
#'                                    ext_ids = stats::setNames(letters, LETTERS)[1:5],
#'                                    user_ids = month.abb[1:6])@c_ids_as_ged, "json2")
FamilyRecord <- S7::new_class(
  "FamilyRecord", 
  parent = Record,
  properties = list(
    facts = S7::new_property(S7::class_list | 
                               S7::new_S3_class("gedcomS7::FamilyFact"),
                             validator = function(value){
                               chk_input_S7classes(value, FamilyFact)
                             }),
    non_events = S7::new_property(S7::class_list | 
                                    S7::new_S3_class("gedcomS7::NonEvent"),
                                  validator = function(value){
                                    chk_input_S7classes(value, NonEvent)
                                  }),
    husb_xref = S7::new_property(S7::class_character,
                                 validator = function(value){
                                   c(
                                     chk_input_size(value, 0, 1),
                                     chk_input_pattern(value, reg_xref(TRUE))
                                   )
                                 }),
    wife_xref = S7::new_property(S7::class_character,
                                 validator = function(value){
                                   c(
                                     chk_input_size(value, 0, 1),
                                     chk_input_pattern(value, reg_xref(TRUE))
                                   )
                                 }),
    chil_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    associations = S7::new_property(S7::class_list | 
                                      S7::new_S3_class("gedcomS7::Association"),
                                    validator = function(value){
                                      chk_input_S7classes(value, Association)
                                    }),
    subm_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    spouse_sealings = S7::new_property(S7::class_list | 
                                         S7::new_S3_class("gedcomS7::SpouseSealing"),
                                    validator = function(value){
                                      chk_input_S7classes(value, SpouseSealing)
                                    }),
    
    c_marriage_date = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in as.iterable(self@facts)){
          if(fact@fact_type == "MARR") return(fact@c_fact_date)
        }
        character()
      }),
    
    c_marriage_place = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in as.iterable(self@facts)){
          if(fact@fact_type == "MARR") return(fact@c_fact_location)
        }
        character()
      }),
    
    c_as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s FAM", self@xref),
          sprintf("1 RESN %s", self@c_restrictions),
          obj_to_ged(self@facts) |> increase_level(by = 1),
          obj_to_ged(self@non_events) |> increase_level(by = 1),
          named_vec_to_ged(self@husb_xref, "HUSB", "PHRASE") |> increase_level(by = 1),
          named_vec_to_ged(self@wife_xref, "WIFE", "PHRASE") |> increase_level(by = 1),
          named_vec_to_ged(self@chil_xrefs, "CHIL", "PHRASE") |> increase_level(by = 1),
          obj_to_ged(self@associations) |> increase_level(by = 1),
          sprintf("1 SUBM %s", self@subm_xrefs),
          obj_to_ged(self@spouse_sealings) |> increase_level(by = 1),
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


parse_record_fam <- function(rec_lines){
  
  rec <- FamilyRecord(
    xref = parse_line_xref(rec_lines[1]),
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

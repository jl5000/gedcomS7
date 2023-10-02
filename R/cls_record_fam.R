
#' Create a family record object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM FAMILY_RECORD.
#' @export
#' @include cls_record.R cls_fact_fam.R cls_non_event.R cls_association.R
#' @tests
#' fct <- list(class_event_fam("MARR", husb_age = "22y", wife_age = "28y 6m",
#'                            date = "22 AUG 1907", place = "Church"))
#' nevent <- list(class_non_event("DIV"))
#' expect_snapshot_value(class_record_fam(xref = "@F2@",
#'                                        facts = fct, non_events = nevent,
#'                                        husb_xref = "@I8@", wife_xref = "@I9@",
#'                                        chil_xrefs = c("@I98@", Eldest = "@I67@"),
#'                                        locked = TRUE,
#'                                        citations = c("@S34@","@S65@"))@as_ged, "json2")
#' expect_error(class_record_fam("REF"), regexp = "@xref is in an invalid format")
#' expect_error(class_record_fam("@1@", unique_ids = letters), regexp = "@unique_ids is in an invalid format")
#' expect_error(class_record_fam("@1@", ext_ids = LETTERS), regexp = "@ext_ids has too few elements")
#' expect_snapshot_value(class_record_fam("@1@",
#'                                    unique_ids = "a95b5007-2ad2-4bac-81b0-7184243c4512",
#'                                    ext_ids = setNames(letters, LETTERS)[1:5],
#'                                    user_ids = month.abb[1:6])@ids, "json2")
class_record_fam <- S7::new_class(
  "class_record_fam", 
  package = "gedcomS7",
  parent = class_record,
  properties = list(
    facts = S7::new_property(S7::class_list | class_fact_fam,
                             validator = function(value){
                               chk_input_S7classes(value, class_fact_fam)
                             }),
    non_events = S7::new_property(S7::class_list | class_non_event,
                                  validator = function(value){
                                    chk_input_S7classes(value, class_non_event)
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
    associations = S7::new_property(S7::class_list | class_association,
                                    validator = function(value){
                                      chk_input_S7classes(value, class_association)
                                    }),
    subm_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    
    marriage_date = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in iterable(self@facts)){
          if(fact@fact_type == "MARR") return(fact@fact_date)
        }
        character()
      }),
    
    marriage_place = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in iterable(self@facts)){
          if(fact@fact_type == "MARR") return(fact@fact_location)
        }
        character()
      }),
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s FAM", self@xref),
          sprintf("1 RESN %s", self@restrictions),
          obj_to_ged(self@facts) |> increase_level(by = 1),
          obj_to_ged(self@non_events) |> increase_level(by = 1),
          named_vec_to_ged(self@husb_xref, "HUSB", "PHRASE") |> increase_level(by = 1),
          named_vec_to_ged(self@wife_xref, "WIFE", "PHRASE") |> increase_level(by = 1),
          named_vec_to_ged(self@chil_xrefs, "CHIL", "PHRASE") |> increase_level(by = 1),
          obj_to_ged(self@associations) |> increase_level(by = 1),
          sprintf("1 SUBM %s", self@subm_xrefs),
          self@ids |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1),
          obj_to_ged(self@media_links, "OBJE") |> increase_level(by = 1),
          obj_to_ged(self@updated) |> increase_level(by = 1),
          obj_to_ged(self@created) |> increase_level(by = 1)
        )
      })
  )
)


extract_record_fam <- function(rec_lines){
  
  rec <- class_record_fam(
    xref = extract_ged_xref(rec_lines[1]),
    facts = extract_facts_fam(rec_lines),
    non_events = extract_non_events(rec_lines),
    husb_xref = extract_vals_and_types(rec_lines, "HUSB"),
    wife_xref = extract_vals_and_types(rec_lines, "WIFE"),
    chil_xrefs = extract_vals_and_types(rec_lines, "CHIL"),
    associations = extract_associations(rec_lines),
    subm_xrefs = find_ged_values(rec_lines, "SUBM")
  )
  
  extract_common_record_elements(rec, rec_lines)
}
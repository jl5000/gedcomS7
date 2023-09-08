
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
class_record_fam <- S7::new_class(
  "class_record_fam", 
  package = "gedcomS7",
  parent = class_record,
  properties = list(
    facts = S7::class_list | class_fact_fam,
    non_events = S7::class_list | class_non_event,
    husb_xref = S7::class_character,
    wife_xref = S7::class_character,
    chil_xrefs = S7::class_character,
    associations = S7::class_list | class_association,
    subm_xrefs = S7::class_character,
    
    marriage_date = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact_type == "MARR") return(fact@fact_date)
        }
        character()
      }),
    
    marriage_place = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
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
  ),
  validator = function(self){
    c(
      chk_input_S7classes(self@facts, "@facts", class_fact_fam),
      chk_input_S7classes(self@non_events, "@non_events", class_non_event),
      chk_input_size(self@husb_xref, "@husb_xref", 0, 1),
      chk_input_pattern(self@husb_xref, "@husb_xref", reg_xref(TRUE)),
      chk_input_size(self@wife_xref, "@wife_xref", 0, 1),
      chk_input_pattern(self@wife_xref, "@wife_xref", reg_xref(TRUE)),
      chk_input_pattern(self@chil_xrefs, "@chil_xrefs", reg_xref(TRUE)),
      chk_input_S7classes(self@associations, "@associations", class_association),
      chk_input_pattern(self@subm_xrefs, "@subm_xrefs", reg_xref(TRUE))
    )
  })


extract_record_fam <- function(rec_lines){
  
  chil_xref <- find_ged_values(rec_lines, "CHIL")
  biol_xref <- adop_xref <- fost_xref <- character()
  for(chil in chil_xref){
    chil_lines <- x@indi[[chil]]
    links <- extract_family_links(chil_lines)
    
    for(lnk in links){
      if(lnk@xref == rec_xref){
        if(is_adop_child_link(lnk)){
          adop_xref <- c(adop_xref, chil)
        } else if(is_fost_child_link(lnk)){
          fost_xref <- c(fost_xref, chil)
        } else if(is_birth_child_link(lnk)){
          biol_xref <- c(biol_xref, chil)
        }
      }
      
    }
  }
  
  rec <- class_record_fam(
    xref = extract_ged_xref(rec_lines[1]),
    husb_xref = find_ged_values(rec_lines, "HUSB"),
    wife_xref = find_ged_values(rec_lines, "WIFE"),
    chil_biol_xref = biol_xref,
    chil_adop_xref = adop_xref,
    chil_fost_xref = fost_xref,
    facts = extract_facts_famg(rec_lines),
    num_children = as.integer(find_ged_values(rec_lines, "NCHI"))
  )
  
  extract_common_record_elements(rec, rec_lines)
}
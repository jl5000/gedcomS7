
#' Create an individual record object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM INDIVIDUAL_RECORD.
#' @export
#' @include cls_record.R cls_personal_name.R cls_fact_indi.R cls_non_event.R cls_association.R
#' @tests
#' nms <- list(class_personal_name("Joe /Bloggs/"),
#'             class_personal_name("Joseph /Bloggs/"))
#' fcts <- list(class_event_indi("BIRT", date = "2005", place = "USA"),
#'              class_event_indi("BIRT", date = "2006", place = "Colorado, USA"),
#'              class_event_indi("DEAT", date = "18 JUN 2020", place = "London, UK"),
#'              class_event_indi("DEAT", date = "2021", place = "UK"))
#' expect_equal(class_record_indi(xref = "@I1@", pers_names = nms)@primary_name, "Joe /Bloggs/")
#' expect_equal(class_record_indi(xref = "@I1@", pers_names = nms)@all_names, c("Joe /Bloggs/","Joseph /Bloggs/"))
#' birt_deat <- class_record_indi(xref = "@I1@", facts = fcts)
#' expect_equal(birt_deat@birth_date, "2005")
#' expect_equal(birt_deat@birth_place, "USA")
#' expect_equal(birt_deat@death_date, "18 JUN 2020")
#' expect_equal(birt_deat@death_place, "London, UK")
#' expect_equal(birt_deat@is_alive, FALSE)
#' expect_snapshot_value(class_record_indi("@I4@", sex = "M", facts = fcts, pers_names = nms,
#'                                         fam_links_chil = "@F132@", 
#'                                         fam_links_spou = "@F67@")@as_ged, "json2")
class_record_indi <- S7::new_class(
  "class_record_indi", 
  package = "gedcomS7",
  parent = class_record,
  properties = list(
    pers_names = S7::new_property(S7::class_list | class_personal_name | S7::class_character,
                                  validator = function(value){
                                    chk_input_S7classes(value, class_personal_name, ".+")
                                  }),
    sex = S7::new_property(S7::class_character, default = "U",
                           validator = function(value){
                             c(
                               chk_input_size(value, 0, 1),
                               chk_input_choice(value, val_sexes())
                             )
                           }),
    facts = S7::new_property(S7::class_list,
                             validator = function(value){
                               chk_input_S7classes(value, class_fact_indi)
                             }),
    non_events = S7::new_property(S7::class_list,
                                  validator = function(value){
                                    chk_input_S7classes(value, class_non_event)
                                  }),
    fam_links_chil = S7::new_property(S7::class_list | class_child_family_link | S7::class_character,
                                      validator = function(value){
                                        chk_input_S7classes(value, class_child_family_link, reg_xref(TRUE))
                                      }),
    fam_links_spou = S7::new_property(S7::class_list | class_spouse_family_link | S7::class_character,
                                      validator = function(value){
                                        chk_input_S7classes(value, class_spouse_family_link, reg_xref(TRUE))
                                      }),
    subm_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    associations = S7::new_property(S7::class_list,
                                    validator = function(value){
                                      chk_input_S7classes(value, class_association)
                                    }),
    alia_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    anci_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    desi_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    
    primary_name = S7::new_property(
      S7::class_character,
      getter = function(self){
        if(length(self@pers_names) == 0){
          character()
        } else {
          obj_to_val(self@pers_names[[1]])
        }
      }),
    
    all_names = S7::new_property(
      S7::class_character,
      getter = function(self){
        vapply(self@pers_names, \(nm){
          obj_to_val(nm)
        }, FUN.VALUE = character(1), USE.NAMES = FALSE)
      }),
    
    desc_short = S7::new_property(
      S7::class_character,
      getter = function(self){
        if(length(self@primary_name) == 0){
          name <- "Unnamed individual"
        } else {
          name <- self@primary_name
        }
        paste0("Individual ", self@xref, ", ", name)
      }),
    
    birth_date = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact_type == "BIRT") return(fact@fact_date)
        }
        character()
      }),
    
    birth_place = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact_type == "BIRT") return(fact@fact_location)
        }
        character()
      }),
    
    is_alive = S7::new_property(
      S7::class_logical,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact_type == "DEAT") return(FALSE)
        }
        TRUE
      }),
    
    death_date = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact_type == "DEAT") return(fact@fact_date)
        }
        character()
      }),
    
    death_place = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact_type == "DEAT") return(fact@fact_location)
        }
        character()
      }),
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s INDI", self@xref),
          sprintf("1 RESN %s", self@restrictions),
          obj_to_ged(self@pers_names, "NAME") |> increase_level(by = 1),
          sprintf("1 SEX %s", self@sex),
          obj_to_ged(self@facts) |> increase_level(by = 1),
          obj_to_ged(self@non_events) |> increase_level(by = 1),
          obj_to_ged(self@fam_links_chil, "FAMC") |> increase_level(by = 1),
          obj_to_ged(self@fam_links_spou, "FAMS") |> increase_level(by = 1),
          sprintf("1 SUBM %s", self@subm_xrefs),
          obj_to_ged(self@associations) |> increase_level(by = 1),
          named_vec_to_ged(self@alia_xrefs, "ALIA", "PHRASE") |> increase_level(by = 1),
          sprintf("1 ANCI %s", self@anci_xrefs),
          sprintf("1 DESI %s", self@desi_xrefs),
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


extract_record_indi <- function(rec_lines){
  
  rec <- class_record_indi(
    xref = extract_ged_xref(rec_lines[1]),
    sex = find_ged_values(rec_lines, "SEX"),
    pers_names = extract_personal_names(rec_lines),
    facts = extract_facts_indi(rec_lines),
    non_events = extract_non_events(rec_lines),
    fam_links_chil = extract_family_links(rec_lines, as_spouse = FALSE),
    fam_links_spou = extract_family_links(rec_lines, as_spouse = TRUE),
    subm_xrefs = find_ged_values(rec_lines, "SUBM"),
    associations = extract_associations(rec_lines),
    alia_xrefs = extract_vals_and_types(rec_lines, "ALIA"),
    anci_xrefs = find_ged_values(rec_lines, "ANCI"),
    desi_xrefs = find_ged_values(rec_lines, "DESI")
  )
  
  extract_common_record_elements(rec, rec_lines)
}
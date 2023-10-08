#' @include cls_validators.R
NULL

#' @export
#' @include cls_date.R cls_time.R cls_place.R cls_note.R cls_citation.R
class_ordinance <- S7::new_class(
  "class_ordinance",
  package = "gedcomS7",
  properties = list( 
    ord_type = S7::new_property(S7::class_character,
                                validator = function(value){
                                  c(
                                    chk_input_size(value, 1, 1),
                                    chk_input_choice(value, val_individual_ordinance_types())
                                  )
                                }),
    date = S7::new_property(S7::class_character | class_date_value,
                            validator = function(value){
                              c(
                                chk_input_size(value, 0, 1),
                                chk_input_pattern(value, reg_date_value())
                              )
                            }),
    temple_name = S7::new_property(S7::class_character,
                                   validator = function(value){
                                     chk_input_size(value, 0, 1, 1)
                                   }),
    place = S7::new_property(S7::class_character | class_place,
                             validator = function(value){
                               chk_input_size(value, 0, 1, 1)
                             }),
    ord_state = S7::new_property(S7::class_character,
                                 validator = function(value){
                                   chk_input_size(value, 0, 1)
                                   chk_input_choice(value, val_ordinance_states())
                                 }),
    state_date = S7::new_property(S7::class_character | class_date_exact, 
                                  validator = function(value){
                                    c(
                                      chk_input_size(value, 0, 1),
                                      chk_input_pattern(value, reg_date_exact())
                                    )
                                  }),
    state_time = S7::new_property(S7::class_character | class_time,
                            validator = function(value){
                              c(
                                chk_input_size(value, 0, 1),
                                chk_input_pattern(value, reg_time())
                              )
                            }),
    note_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    notes = S7::new_property(S7::class_list | class_note | S7::class_character,
                             validator = function(value){
                               chk_input_S7classes(value, class_note, ".+")
                             }),
    citations = S7::new_property(S7::class_list | class_citation | S7::class_character,
                                 validator = function(value){
                                   chk_input_S7classes(value, class_citation, reg_xref(TRUE))
                                 }),
    fam_xref = S7::new_property(S7::class_character,
                                validator = function(value){
                                  c(
                                    chk_input_size(value, 0, 1),
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  )
                                }),
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s", self@ord_type),
          obj_to_ged(self@date, "DATE") |> increase_level(by = 1),
          sprintf("1 TEMP %s", self@temple_name),
          obj_to_ged(self@place, "PLAC") |> increase_level(by = 1),
          sprintf("1 STAT %s", self@ord_state),
          sprintf("2 DATE %s", obj_to_val(self@state_date)),
          sprintf("3 TIME %s", obj_to_val(self@state_time)),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1),
          sprintf("1 FAMC %s", self@fam_xref)
        )
      })
  ),
  validator = function(self){
    errs <- NULL
    if(length(self@ord_state) == 1){
      if(self@ord_state == "BIC" && self@ord_type != "SLGC")
        errs <- c(errs, "An @ord_state of BIC is only for @ord_type of SLGC")
      
      if(self@ord_state %in% c("CANCELED","DNS_CAN") && self@ord_type != "SLGS")
        errs <- c(errs, "An @ord_state of CANCELED/DNS_CAN is only for @ord_type of SLGS")
      
      if(self@ord_state %in% c("CHILD","INFANT") && self@ord_type == "SLGC")
        errs <- c(errs, "An @ord_state of CHILD/INFANT cannot be used for @ord_type of SLGC")
      
      if(self@ord_state == "DNS" && !self@ord_type %in% c("SLGC","SLGS"))
        errs <- c(errs, "An @ord_state of DNS is only for @ord_type of SLGC/SLGS")
    }
    
    if(length(self@fam_xref) == 0 && self@ord_type == "SLGC")
      errs <- c(errs, "A child sealing requires a @fam_xref")
    
    if(length(self@fam_xref) == 1 && self@ord_type != "SLGC")
      errs <- c(errs, "A @fam_xref is not required for this @ord_type")
    
    c(
      errs,
      chk_input_parents(self@ord_state, "@ord_state", self@state_date, "@state_date"),
      chk_input_parents(self@state_date, "@state_date", self@ord_state, "@ord_state"),
      chk_input_parents(self@state_time, "@state_time", self@state_date, "@state_date")
    )
  }
)

#' @export
class_spouse_sealing <- S7::new_class(
  "class_spouse_sealing",
  package = "gedcomS7",
  parent = class_ordinance,
  properties = list( 
    ord_type = S7::new_property(S7::class_character,
                                getter = function(self) "SLGS"),
    fam_xref = S7::new_property(S7::class_character,
                                validator = function(value){
                                  chk_input_size(value, 0, 0)
                                })
  )
)


extract_ordinances <- function(rec_lines){
  ord_lst <- find_ged_values(rec_lines, return_list = TRUE, 
                             tag = paste(c(val_individual_ordinance_types(),"SLGS"),
                                         collapse = "|"))
  if(length(ord_lst) == 0) return(list())
  
  lapply(ord_lst, \(x){
    tag <- extract_ged_tag(x[1])
    
    if(tag == "SLGS"){
      ord <- class_spouse_sealing()
    } else {
      ord <- class_ordinance(
        ord_type = tag,
        fam_xref = find_ged_values(x, c(tag,"FAMC"))
      )
    }
    
    S7::props(ord) <- list(
      date = extract_date_value(x, tag),
      temple_name = find_ged_values(x, c(tag,"TEMP")),
      place = extract_place(x, tag),
      ord_state = find_ged_values(x, c(tag,"STAT")),
      state_date = find_ged_values(x, c(tag,"STAT","DATE")),
      state_time = find_ged_values(x, c(tag,"STAT","DATE","TIME")),
      note_xrefs = find_ged_values(x, c(tag,"SNOTE")),
      notes = extract_notes(x, tag),
      citations = extract_citations(x, tag)
    )
    
    ord
  })
}
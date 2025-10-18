
#' Create an individual ordinance object
#' 
#' @inheritParams prop_definitions 
#' @param ord_type A value from `val_individual_ordinance_types()`.
#' 
#' @returns An S7 object representing a GEDCOM LDS_INDIVIDUAL_ORDINANCE.
#' @export
Ordinance <- S7::new_class(
  "Ordinance",
  parent = GedcomS7class,
  properties = list( 
    ord_type = prop_char(1, 1, choices = val_individual_ordinance_types()),
    date = prop_char(0, 1, pattern = reg_date_value(), S7class_names = "DateValue"),
    temple_name = prop_char(0, 1, 1),
    place = prop_S7obj("place", Place),
    ord_state = prop_char(0, 1),
    state_date = prop_char(0, 1, pattern = reg_date_exact(), S7class_names = "DateExact"),
    state_time = prop_char(0, 1, pattern = reg_time(), S7class_names = "Time"),
    note_xrefs = prop_char(pattern = reg_xref(TRUE)),
    notes = prop_S7list("notes", Note),
    citations = prop_S7list("citations", SourceCitation),
    fam_xref = prop_char(0, 1, pattern = reg_xref(TRUE)),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s", self@ord_type),
          as_ged(self@date, "DATE") |> level_up(1),
          sprintf("1 TEMP %s", self@temple_name),
          as_ged(self@place) |> level_up(1),
          sprintf("1 STAT %s", self@ord_state),
          sprintf("2 DATE %s", as_val(self@state_date)),
          sprintf("3 TIME %s", as_val(self@state_time)),
          notes_ged(self@notes, self@note_xrefs) |> level_up(1),
          as_ged(self@citations) |> level_up(1),
          sprintf("1 FAMC %s", self@fam_xref)
        )
      })
  ),
  validator = function(self){
    errs <- chk_input_choice(self@ord_state, val_ordinance_states(self@ord_type))

    if(length(self@fam_xref) == 0 && self@ord_type == "SLGC")
      errs <- c(errs, "A child sealing requires a @fam_xref")
    
    if(length(self@fam_xref) == 1 && self@ord_type != "SLGC")
      errs <- c(errs, "A @fam_xref is not required for this @ord_type")
    
    if(is.character(self@date) && isTRUE(self@date == ""))
      errs <- c(errs, "A blank @date requires a @date_phrase and therefore requires a DateValue object.")
    
    c(
      errs,
      chk_input_parents(self@ord_state, "@ord_state", self@state_date, "@state_date"),
      chk_input_parents(self@state_date, "@state_date", self@ord_state, "@ord_state"),
      chk_input_parents(self@state_time, "@state_time", self@state_date, "@state_date")
    )
  }
)


#' Create a spouse sealing object
#' 
#' @inheritParams prop_definitions 
#'  
#' @returns An S7 object representing a GEDCOM LDS_SPOUSE_SEALING.
#' @export
SpouseSealing <- S7::new_class(
  "SpouseSealing",
  parent = GedcomS7class,
  properties = list( 
    ord_type = S7::new_property(S7::class_character,
                                getter = function(self) "SLGS"),
    date = prop_char(0, 1, pattern = reg_date_value(), S7class_names = "DateValue"),
    temple_name = prop_char(0, 1, 1),
    place = prop_S7obj("place", Place),
    ord_state = prop_char(0, 1, choices = val_ordinance_states("SLGS")),
    state_date = prop_char(0, 1, pattern = reg_date_exact(), S7class_names = "DateExact"),
    state_time = prop_char(0, 1, pattern = reg_time(), S7class_names = "Time"),
    note_xrefs = prop_char(pattern = reg_xref(TRUE)),
    notes = prop_S7list("notes", Note),
    citations = prop_S7list("citations", SourceCitation),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s", self@ord_type),
          as_ged(self@date, "DATE") |> level_up(1),
          sprintf("1 TEMP %s", self@temple_name),
          as_ged(self@place) |> level_up(1),
          sprintf("1 STAT %s", self@ord_state),
          sprintf("2 DATE %s", as_val(self@state_date)),
          sprintf("3 TIME %s", as_val(self@state_time)),
          notes_ged(self@notes, self@note_xrefs) |> level_up(1),
          as_ged(self@citations) |> level_up(1)
        )
      })
  ),
  validator = function(self){
    errs <- NULL
    if(is.character(self@date) && isTRUE(self@date == ""))
      errs <- c(errs, "A blank @date requires a @date_phrase and therefore requires a DateValue object.")
    
    c(
      errs,
      chk_input_parents(self@ord_state, "@ord_state", self@state_date, "@state_date"),
      chk_input_parents(self@state_date, "@state_date", self@ord_state, "@ord_state"),
      chk_input_parents(self@state_time, "@state_time", self@state_date, "@state_date")
    )
  }
)


parse_ordinances <- function(rec_lines){
  ord_lst <- find_ged_values(rec_lines, return_list = TRUE, 
                             tag = paste(c(val_individual_ordinance_types(),"SLGS"),
                                         collapse = "|"))

  lapply(ord_lst, \(x){
    tag <- parse_line_tag(x[1])
    
    if(tag == "SLGS"){
      ord <- SpouseSealing()
    } else {
      ord <- Ordinance(
        ord_type = tag,
        fam_xref = find_ged_values(x, c(tag,"FAMC"))
      )
    }
    
    S7::props(ord) <- list(
      date = parse_date_value(x, tag),
      temple_name = find_ged_values(x, c(tag,"TEMP")),
      place = parse_place(x, tag),
      ord_state = find_ged_values(x, c(tag,"STAT")),
      state_date = find_ged_values(x, c(tag,"STAT","DATE")),
      state_time = find_ged_values(x, c(tag,"STAT","DATE","TIME")),
      note_xrefs = find_ged_values(x, c(tag,"SNOTE")),
      notes = parse_notes(x, tag),
      citations = parse_citations(x, tag)
    )
    
    ord
  })
}

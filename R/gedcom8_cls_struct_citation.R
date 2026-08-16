

Citation <- S7::new_class(
  "Citation",
  parent = GedcomS7class,
  properties = list(
    sour_xref = prop_char(1, 1, pattern = reg_xref(TRUE)),
    where = prop_char(0, 1, 1),
    extract_date = prop_char(0, 1, pattern = reg_date_value(), S7class_names = "DateValue"),
    extract_text = prop_S7list("source_text", TranslationText),
    originator = prop_char(0, 1, 1),
    full_title = prop_char(0, 1, 1),
    short_title = prop_char(0, 1, 1),
    created_date,
    published_date,
    accessed_date,
    recorded_date,
    #repository,
    citation_text,
    #transcript,
    
    
    certainty = prop_char(0, 1, choices = val_certainty(), casting_name = "certainty"),
    media_links = prop_S7list("media_links", MediaLink),
    note_xrefs = prop_char(pattern = reg_xref(TRUE)),
    notes = prop_S7list("notes", Note),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          as_ged(self@sour_xref, "SOUR", 0),
          as_ged(self@where, "PAGE", 1),
          rep("1 DATA", length(self@date) + 
                length(self@source_text) > 0),
          as_ged(self@date, "DATE", 2),
          as_ged(self@source_text, 2) |> 
            gsub(pattern = "(^\\d) TRAN ", replacement = "\\1 TEXT "),
          as_ged(self@fact_type, "EVEN", 1),
          as_ged(self@fact_phrase, "PHRASE", 2),
          as_ged(self@role, "ROLE", 2),
          as_ged(self@role_phrase, "PHRASE", 3),
          as_ged(self@certainty, "QUAY", 1),
          as_ged(self@media_links, 1),
          notes_ged(self@notes, self@note_xrefs, 1)
        ) 
      })
  ),
  
  validator = function(self){
    errs <- NULL
    if(is.character(self@date) && isTRUE(self@date == ""))
      errs <- c(errs, "A blank @date requires a @date_phrase and therefore requires a DateValue object.")
    
    c(
      errs,
      chk_input_parents(self@fact_phrase, "@fact_phrase", self@fact_type, "@fact_type"),
      chk_input_parents(self@role, "@role", self@fact_type, "@fact_type"),
      chk_input_parents(self@role_phrase, "@role_phrase", self@role, "@role"),
      chk_input_phrase(self@role_phrase, "@role_phrase",
                       self@role, "@role", "OTHER")
    )
  }
  
)



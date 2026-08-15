

TemplateRecord <- S7::new_class(
  "TemplateRecord", 
  parent = GedcomS7class,
  properties = list(
    XREF = prop_char(1, 1, pattern = reg_xref(TRUE), default = new_xref()),
    type,
    title,
    phrase,
    citation,
    subm_xrefs,
    subm_notes,
    origin,
    certainty = prop_char(0, 1, choices = val_certainty(), casting_name = "certainty"),
    language,
    date = prop_char(0, 1, pattern = reg_date_value(), S7class_names = "DateValue"),
    place_xref,
    facts,
    agency,
    agency_contact,
    claim_xrefs,
    eldest_xref,
    transfers,
    misc_data,
    group_xrefs,
    asset_xrefs,
    
    
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          as_ged("TEMPLATE", self@XREF),
          as_ged(self@type, "TYPE", 1),
          as_ged(self@title, "TITL", 1),
          as_ged(self@phrase, "PHRASE", 2),
          #subm
          restrictions_ged(self@confidential, self@locked, self@private, 1),
          as_ged(self@origin, "TORIGIN", 1),
          as_ged(self@certainty, "QUAY", 1),
          as_ged(self@language, "LANG", 1),
          as_ged(self@date, "DATE", 1),
          
          as_ged(self@repo_name, "NAME", 1),
          contacts_ged(self@address, self@phone_numbers, self@emails,
                       self@faxes, self@web_pages, 1),
          notes_ged(self@notes, self@note_xrefs, 1),
          identifiers_ged(self@user_ids, self@unique_ids, self@ext_ids, 1),
          audit_ged(self@updated, self@created, 1)
        )
      })
  ),
  validator = function(self){
    if(length(self@citations) > 0)
      return("This record does not use @citations")
    
    if(length(self@media_links) > 0)
      return("This record does not use @media_links")
  }
)

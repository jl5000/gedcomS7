

PlaceRecord <- S7::new_class(
  "PlaceRecord", 
  parent = GedcomS7class,
  properties = list(
    XREF = prop_char(1, 1, pattern = reg_xref(TRUE), default = new_xref()),
    name = prop_char(1, 1, 1),
    language = prop_char(0, 1, 1),
    place_type,
    
    periods = prop_S7list("periods", Period),
    notes = prop_S7list("notes", Note),
    created = prop_S7obj("created", CreationDate),
    updated = prop_S7obj("updated", ChangeDate),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          as_ged("SPLAC", self@XREF) |> paste(self@name),
          
          notes_ged(self@notes, self@note_xrefs, 1),
          
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



Period <- S7::new_class(
  "Period", 
  parent = GedcomS7class,
  properties = list(
    
  )
)


PlaceLink <- S7::new_class(
  "PlaceLink", 
  parent = GedcomS7class,
  properties = list(
    place_xref = prop_char(1, 1, pattern = reg_xref(TRUE)),
    place_phrase = prop_char(0, 1, 1),
    notes = prop_S7list("notes", Note)
  )
)



PlaceName <- S7::new_class(
  "PlaceName", 
  parent = GedcomS7class,
  properties = list(
    
  )
)


PlaceLocation <- S7::new_class(
  "PlaceLocation", 
  parent = GedcomS7class,
  properties = list(
    lat_long = prop_char(0, 1, pattern = sprintf("^%s %s$", reg_latitude(), reg_longitude())),
    radius = S7::class_double,
    address = prop_S7obj("address", Address),
    ext_ids = prop_char(min_char = 1, names_required = TRUE), # definitely named
    gov_id = prop_char(0, 1, 1, 14),
    maid_id = prop_char(0, 1, 1, 8),
    postal_code = prop_char(0, 1, 1, 10),
  )
)

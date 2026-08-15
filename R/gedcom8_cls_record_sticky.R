
StickyRecord <- S7::new_class(
  "StickyRecord", 
  parent = Record_G8,
  properties = list(
    repo_name = prop_char(1, 1, 1),
    address = prop_S7obj("address", Address),
    phone_numbers = prop_char(min_char = 1),
    emails = prop_char(min_char = 1),
    faxes = prop_char(min_char = 1),
    web_pages = prop_char(min_char = 1),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          as_ged("REPO", self@XREF),
          restrictions_ged(self@confidential, self@locked, self@private, 1),
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

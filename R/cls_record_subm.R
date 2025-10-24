
#' Create a submitter record object
#' 
#' @inheritParams prop_definitions 
#' @param subm_name The name of the submitter.
#' @param citations Not used.
#' @param languages A character vector of language tags as defined in BCP 47.
#' 
#' @returns An S7 object representing a GEDCOM SUBMITTER_RECORD.
#' @export
SubmitterRecord <- S7::new_class(
  "SubmitterRecord", 
  parent = Record,
  properties = list(
    subm_name = prop_char(1, 1, 1),
    address = prop_S7obj("address", Address),
    phone_numbers = prop_char(min_char = 1),
    emails = prop_char(min_char = 1),
    faxes = prop_char(min_char = 1),
    web_pages = prop_char(min_char = 1),
    languages = prop_char(min_char = 1),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          as_ged("SUBM", self@XREF),
          restrictions_ged(self@confidential, self@locked, self@private, 1),
          as_ged(self@subm_name, "NAME", 1),
          contacts_ged(self@address, self@phone_numbers, self@emails,
                          self@faxes, self@web_pages, 1),
          as_ged(self@media_links, 1),
          as_ged(self@languages, "LANG", 1),
          identifiers_ged(self@user_ids, self@unique_ids, self@ext_ids, 1),
          notes_ged(self@notes, self@note_xrefs, 1),
          audit_ged(self@updated, self@created, 1)
        )
      })
  ),
  validator = function(self){
    if(length(self@citations) > 0)
      return("This record does not use @citations")
  }
)

parse_record_subm <- function(rec_lines){
  
  rec <- SubmitterRecord(
    XREF = parse_line_xref(rec_lines[1]),
    subm_name = find_ged_values(rec_lines, "NAME"),
    address = parse_address(rec_lines),
    phone_numbers = find_ged_values(rec_lines, "PHON"),
    emails = find_ged_values(rec_lines, "EMAIL"),
    faxes = find_ged_values(rec_lines, "FAX"),
    web_pages = find_ged_values(rec_lines, "WWW"),
    languages = find_ged_values(rec_lines, "LANG")
  )
  
  parse_common_record_elements(rec, rec_lines)
}


S7::method(summary, SubmitterRecord) <- function(object, ...){
  exdent <- 15
  to_console("XREF:", object@XREF, exdent)
  to_console("Submitter:", object@subm_name, exdent)
  to_console("Languages:", toString(object@languages), exdent)
  if(!is.null(object@address)) 
    to_console("Address:", object@address@full, exdent)
  
  to_console_list("Phone Numbers:", object@phone_numbers, exdent)
  to_console_list("Fax Numbers:", object@faxes, exdent)
  to_console_list("Emails:", object@emails, exdent)
  to_console_list("Web Pages:", object@web_pages, exdent)
  cat("\n")
  to_console("Media Links:", length(object@media_links), exdent)
  to_console("Notes:", length(object@notes) + length(object@note_xrefs), exdent)
  cat("\n")
  print_record_summary(object)
}


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
          sprintf("0 %s SUBM", self@XREF),
          sprintf("1 RESN %s", self@RESTRICTIONS),
          sprintf("1 NAME %s", self@subm_name),
          obj_to_ged(self@address, "ADDR") |> increase_level(by = 1),
          sprintf("1 PHON %s", self@phone_numbers),
          sprintf("1 EMAIL %s", self@emails),
          sprintf("1 FAX %s", self@faxes),
          sprintf("1 WWW %s", self@web_pages),
          obj_to_ged(self@media_links, "OBJE") |> increase_level(by = 1),
          sprintf("1 LANG %s", self@languages),
          self@GEDCOM_IDENTIFIERS |> increase_level(by = 1),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@updated) |> increase_level(by = 1),
          obj_to_ged(self@created) |> increase_level(by = 1)
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

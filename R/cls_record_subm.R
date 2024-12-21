
#' Create a submitter record object
#' 
#' @inheritParams prop_definitions 
#' @param subm_name The name of the submitter.
#' @param citations Not used.
#' @param languages A character vector of language tags as defined in BCP 47.
#' 
#' @return An S7 object representing a GEDCOM SUBMITTER_RECORD.
#' @export
SubmitterRecord <- S7::new_class(
  "SubmitterRecord", 
  parent = Record,
  properties = list(
    subm_name = S7::new_property(S7::class_character,
                                 validator = function(value){
                                   chk_input_size(value, 1, 1, 1)
                                 }),
    address = S7::new_property(S7::class_character | 
                                 S7::new_S3_class("gedcomS7::Address"),
                               validator = function(value){
                                 chk_input_size(value, 0, 1, 1)
                               }),
    phone_numbers = S7::new_property(S7::class_character,
                                     validator = function(value){
                                       chk_input_size(value, min_val = 1)
                                     }),
    emails = S7::new_property(S7::class_character,
                              validator = function(value){
                                chk_input_size(value, min_val = 1)
                              }),
    faxes = S7::new_property(S7::class_character,
                             validator = function(value){
                               chk_input_size(value, min_val = 1)
                             }),
    web_pages = S7::new_property(S7::class_character,
                                 validator = function(value){
                                   chk_input_size(value, min_val = 1)
                                 }),
    languages = S7::new_property(S7::class_character,
                                 validator = function(value){
                                   #TODO: language patterns
                                 }),
    
    c_as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s SUBM", self@xref),
          sprintf("1 RESN %s", self@c_restrictions),
          sprintf("1 NAME %s", self@subm_name),
          obj_to_ged(self@address, "ADDR") |> increase_level(by = 1),
          sprintf("1 PHON %s", self@phone_numbers),
          sprintf("1 EMAIL %s", self@emails),
          sprintf("1 FAX %s", self@faxes),
          sprintf("1 WWW %s", self@web_pages),
          obj_to_ged(self@media_links, "OBJE") |> increase_level(by = 1),
          sprintf("1 LANG %s", self@languages),
          self@c_ids_as_ged |> increase_level(by = 1),
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
    xref = parse_line_xref(rec_lines[1]),
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

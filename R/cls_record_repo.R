
#' Create a repository record object
#' 
#' @inheritParams prop_definitions 
#' @param citations Not used.
#' @param media_links Not used.
#' @return An S7 object representing a GEDCOM REPOSITORY_RECORD.
#' @export
#' @include cls_record.R cls_address.R
class_record_repo <- S7::new_class(
  "class_record_repo", 
  package = "gedcomS7",
  parent = class_record,
  properties = list(
    repo_name = S7::new_property(S7::class_character,
                                 validator = function(value){
                                   chk_input_size(value, 1, 1, 1)
                                 }),
    address = S7::new_property(S7::class_character | class_address,
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
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s REPO", self@xref),
          sprintf("1 RESN %s", self@restrictions),
          sprintf("1 NAME %s", self@repo_name),
          obj_to_ged(self@address, "ADDR") |> increase_level(by = 1),
          sprintf("1 PHON %s", self@phone_numbers),
          sprintf("1 EMAIL %s", self@emails),
          sprintf("1 FAX %s", self@faxes),
          sprintf("1 WWW %s", self@web_pages),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_xrefs),
          self@ids |> increase_level(by = 1),
          obj_to_ged(self@updated) |> increase_level(by = 1),
          obj_to_ged(self@created) |> increase_level(by = 1)
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


parse_record_repo <- function(rec_lines){
  
  rec <- class_record_repo(
    xref = parse_line_xref(rec_lines[1]),
    repo_name = find_ged_values(rec_lines, "NAME"),
    address = parse_address(rec_lines),
    phone_numbers = find_ged_values(rec_lines, "PHON"),
    emails = find_ged_values(rec_lines, "EMAIL"),
    faxes = find_ged_values(rec_lines, "FAX"),
    web_pages = find_ged_values(rec_lines, "WWW")
  )
  
  parse_common_record_elements(rec, rec_lines)
}

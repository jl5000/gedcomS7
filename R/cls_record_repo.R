
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
    repo_name = S7::class_character,
    address = S7::class_character | class_address,
    phone_numbers = S7::class_character,
    emails = S7::class_character,
    faxes = S7::class_character,
    web_pages = S7::class_character,
    
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
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          self@ids |> increase_level(by = 1),
          obj_to_ged(self@updated) |> increase_level(by = 1),
          obj_to_ged(self@created) |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@repo_name, "@repo_name", 1, 1, 1),
      chk_input_size(self@address, "@address", 0, 1),
      chk_input_size(self@phone_numbers, "@phone_numbers", min_val = 1),
      chk_input_size(self@emails, "@emails", min_val = 1),
      chk_input_size(self@faxes, "@faxes", min_val = 1),
      chk_input_size(self@web_pages, "@web_pages", min_val = 1),
      chk_input_size(self@citations, "@citations", 0, 0),
      chk_input_size(self@media_links, "@media_links", 0, 0)
    )
  }
)


extract_record_repo <- function(rec_lines){
  
  rec <- class_record_repo(
    xref = extract_ged_xref(rec_lines[1]),
    repo_name = find_ged_values(rec_lines, "NAME"),
    address = extract_address(rec_lines),
    phone_numbers = find_ged_values(rec_lines, "PHON"),
    emails = find_ged_values(rec_lines, "EMAIL"),
    faxes = find_ged_values(rec_lines, "FAX"),
    web_pages = find_ged_values(rec_lines, "WWW")
  )
  
  extract_common_record_elements(rec, rec_lines)
}
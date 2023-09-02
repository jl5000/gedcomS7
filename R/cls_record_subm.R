
#' Create a submitter record object
#' 
#' @inheritParams prop_definitions 
#' @param citations Not used.
#' @return An S7 object representing a GEDCOM SUBMITTER_RECORD.
#' @export
#' @include cls_record.R cls_address.R
class_record_subm <- S7::new_class(
  "class_record_subm", 
  package = "gedcomS7",
  parent = class_record,
  properties = list(
    subm_name = S7::class_character,
    address = S7::class_character | class_address,
    phone_numbers = S7::class_character,
    emails = S7::class_character,
    faxes = S7::class_character,
    web_pages = S7::class_character,
    languages = S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s SUBM", self@xref),
          sprintf("1 RESN %s", self@restrictions),
          sprintf("1 NAME %s", self@subm_name),
          obj_to_ged(self@address, "ADDR") |> increase_level(by = 1),
          sprintf("1 PHON %s", self@phone_numbers),
          sprintf("1 EMAIL %s", self@emails),
          sprintf("1 FAX %s", self@faxes),
          sprintf("1 WWW %s", self@web_pages),
          obj_to_ged(self@media_links, "OBJE") |> increase_level(by = 1),
          sprintf("1 LANG %s", self@languages),
          self@ids |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          obj_to_ged(self@updated) |> increase_level(by = 1),
          obj_to_ged(self@created) |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@subm_name, "@subm_name", 1, 1, 1),
      chk_input_size(self@address, "@address", 0, 1),
      chk_input_size(self@phone_numbers, "@phone_numbers", min_val = 1),
      chk_input_size(self@emails, "@emails", min_val = 1),
      chk_input_size(self@faxes, "@faxes", min_val = 1),
      chk_input_size(self@web_pages, "@web_pages", min_val = 1),
      #TODO: language patterns
      chk_input_size(self@citations, "@citations", 0, 0)
    )
  }
)


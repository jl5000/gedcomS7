

#' @export
#' @include cls_record.R cls_address.R cls_note.R cls_media_link.R
class_record_subm <- S7::new_class(
  "class_record_subm", 
  package = "gedcomS7",
  parent = class_record,
  properties = list(
    name = S7::class_character,
    address = S7::class_character | class_address,
    phone_numbers = S7::class_character,
    emails = S7::class_character,
    faxes = S7::class_character,
    web_pages = S7::class_character,
    media_links = S7::class_list | class_media_link | S7::class_character,
    language = S7::class_character,
    note_uids = S7::class_character,
    notes = S7::class_list | class_note | S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s SUBM", self@prim_uid),
          sprintf("1 RESN %s", self@restrictions),
          sprintf("1 NAME %s", self@name),
          obj_to_ged(self@address) |> increase_level(by = 1),
          sprintf("1 PHON %s", self@phone_numbers),
          sprintf("1 EMAIL %s", self@emails),
          sprintf("1 FAX %s", self@faxes),
          sprintf("1 WWW %s", self@web_pages),
          obj_to_ged(self@media_links, "OBJE") |> increase_level(by = 1),
          sprintf("1 LANG %s", self@language),
          self@ids |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_uids),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          obj_to_ged(self@updated) |> increase_level(by = 1),
          obj_to_ged(self@created) |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@name, "@name", 1, 1, 1),
      chk_input_size(self@address, "@address", 0, 1),
      chk_input_size(self@phone_numbers, "@phone_numbers", min_val = 1),
      chk_input_size(self@emails, "@emails", min_val = 1),
      chk_input_size(self@faxes, "@faxes", min_val = 1),
      chk_input_size(self@web_pages, "@web_pages", min_val = 1),
      #TODO: language pattern
      chk_input_pattern(self@note_uids, "@note_uids", reg_uuid(TRUE)),
      chk_input_S7classes(self@media_links, "@media_links", class_media_link, reg_uuid(TRUE)),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+")
    )
  }
)


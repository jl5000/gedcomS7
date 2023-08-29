
#' @export
#' @include cls_record.R cls_fact.R cls_non_event.R cls_association.R cls_note.R
#' cls_citation.R cls_media_link.R
class_record_fam <- S7::new_class(
  "class_record_fam", 
  package = "gedcomS7",
  parent = class_record,
  properties = list(
    facts = S7::class_list | class_fact_fam,
    non_events = S7::class_list | class_non_event,
    husb_xref = S7::class_character,
    wife_xref = S7::class_character,
    chil_xrefs = S7::class_character,
    associations = S7::class_list | class_association,
    subm_xrefs = S7::class_character,
    note_xrefs = S7::class_character,
    notes = S7::class_list | class_note | S7::class_character,
    citations = S7::class_list | class_citation | S7::class_character,
    media_links = S7::class_list | class_media_link | S7::class_character,
    
    relationship_date = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact == "MARR") return(fact@fact_date)
        }
        character()
      }),
    
    relationship_place = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact == "MARR") return(fact@fact_location)
        }
        character()
      }),
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s FAM", self@prim_uid),
          sprintf("1 RESN %s", self@restrictions),
          obj_to_ged(self@facts) |> increase_level(by = 1),
          obj_to_ged(self@non_events) |> increase_level(by = 1),
          named_vec_to_ged(self@husb_uid, "HUSB", "PHRASE") |> increase_level(by = 1),
          named_vec_to_ged(self@wife_uid, "WIFE", "PHRASE") |> increase_level(by = 1),
          named_vec_to_ged(self@chil_uids, "CHIL", "PHRASE") |> increase_level(by = 1),
          obj_to_ged(self@associations) |> increase_level(by = 1),
          sprintf("1 SUBM %s", self@subm_uids),
          self@ids |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_uids),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1),
          obj_to_ged(self@media_links, "OBJE") |> increase_level(by = 1),
          obj_to_ged(self@updated) |> increase_level(by = 1),
          obj_to_ged(self@created) |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      
      chk_input_size(self@husb_uid, "@husb_uid", 0, 1),
      chk_input_size(self@wife_uid, "@wife_uid", 0, 1),
      chk_input_pattern(self@husb_uid, "@husb_uid", reg_uuid(TRUE)),
      chk_input_pattern(self@wife_uid, "@wife_uid", reg_uuid(TRUE)),
      chk_input_pattern(self@chil_uids, "@chil_uids", reg_uuid(TRUE)),
      chk_input_pattern(self@subm_uids, "@subm_uids", reg_uuid(TRUE)),
      chk_input_pattern(self@note_uids, "@note_uids", reg_uuid(TRUE)),
      chk_input_S7classes(self@facts, "@facts", class_fact_fam),
      chk_input_S7classes(self@non_events, "@non_events", class_non_event),
      chk_input_S7classes(self@associations, "@associations", class_association),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+"),
      chk_input_S7classes(self@citations, "@citations", class_citation, reg_uuid(TRUE)),
      chk_input_S7classes(self@media_links, "@media_links", class_media_link, reg_uuid(TRUE))
    )
  })

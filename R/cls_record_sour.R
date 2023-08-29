
#' Create a repository citation object
#' 
#' @inheritParams prop_definitions
#' @return An S7 object representing a GEDCOM SOURCE_REPOSITORY_CITATION.
#' @export
#' @include cls_note.R
#' @tests
#' expect_snapshot_value(class_repository_citation()@as_ged, "json2")
#' expect_snapshot_value(class_repository_citation(notes = "Local library",
#'                                                 call_numbers = c("ABC","123"))@as_ged, "json2")
class_repository_citation <- S7::new_class(
  "class_repository_citation",
  package = "gedcomS7",
  properties = list(
    repo_xref = S7::new_property(S7::class_character, default = "@VOID@"),
    notes = S7::class_list | class_note | S7::class_character,
    note_xrefs = S7::class_character,
    call_numbers = S7::class_character,
    # media = S7::class_character, TODO: too much nesting
    # media_phrase = S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 REPO %s", self@repo_xref),
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          sprintf("1 CALN %s", self@call_numbers)
          # sprintf("2 MEDI %s", self@media),
          # sprintf("3 PHRASE %s", self@media_phrase)
        )
      })
  ),
  
  validator = function(self){
    c(
      chk_input_size(self@repo_xref, "@repo_xref", 1, 1),
      chk_input_pattern(self@repo_xref, "@repo_xref", reg_xref(TRUE)),
      chk_input_pattern(self@note_xrefs, "@note_xrefs", reg_xref(TRUE)),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+"),
      chk_input_size(self@call_numbers, "@call_numbers", min_val = 1)
    )
  }
)


#' Create an object recording facts covered in a source record
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM SOUR.EVEN structure.
#' @export
#' @include cls_date.R cls_place.R
#' @tests
#' expect_error(class_facts_recorded("birth"), regexp = "@fact_types is in an invalid format")
#' expect_error(class_facts_recorded("BIRT "), regexp = "@fact_types is in an invalid format")
#' expect_error(class_facts_recorded("BIRT,DEAT"), regexp = "@fact_types is in an invalid format")
#' expect_error(class_facts_recorded("BIRT, DEAT", date_period = "2006"), 
#'                                   regexp = "@date_period is in an invalid format")
#' expect_snapshot_value(class_facts_recorded("BIRT")@as_ged, "json2")
#' expect_snapshot_value(class_facts_recorded("BIRT, DEAT",
#'                                            date_period = "FROM 2007 TO 2010")@as_ged, "json2")
#' expect_snapshot_value(class_facts_recorded("BIRT, DEAT",
#'                                            date_period = "FROM 2007 TO 2010",
#'                                            date_phrase = "sometime",
#'                                            territory = "somewhere")@as_ged, "json2")
class_facts_recorded <- S7::new_class(
  "class_facts_recorded",
  package = "gedcomS7",
  properties = list(
    fact_types = S7::class_character,
    date_period = S7::class_character | class_date_period,
    date_phrase = S7::class_character,
    territory = S7::class_character | class_place,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 EVEN %s", self@fact_types),
          sprintf("1 DATE %s", obj_to_val(self@date_period)) |> trimws(),
          sprintf("2 PHRASE %s", self@date_phrase),
          obj_to_ged(self@territory, "PLAC") |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@fact_types, "@fact_types", 1, 1),
      chk_input_pattern(self@fact_types, "@fact_types", sprintf("^(%s)(, (%s))*$", 
                                                                paste(val_fact_types(), collapse = "|"),
                                                                paste(val_fact_types(), collapse = "|"))),
      chk_input_size(self@date_period, "@date_period", 0, 1),
      chk_input_pattern(self@date_period, "@date_period", reg_date_period()),
      chk_input_size(self@date_phrase, "@date_phrase", 0, 1, 1),
      chk_input_size(self@territory, "@territory", 0, 1),
      chk_input_pattern(self@territory, "@territory", ".+")
    )
  })


#' @export
#' @include cls_record.R cls_note.R cls_translation.R cls_note.R cls_media_link.R
class_record_sour <- S7::new_class(
  "class_record_sour", 
  package = "gedcomS7",
  parent = class_record,
  properties = list(
    facts_recorded = S7::class_list | class_facts_recorded | S7::class_character,
    responsible_agency = S7::class_character,
    data_note_uids = S7::class_character,
    data_notes = S7::class_list | class_note | S7::class_character,
    originator = S7::class_character,
    full_title = S7::class_character,
    short_title = S7::class_character,
    publication_facts = S7::class_character,
    source_text = NULL | class_translation_txt,
    repo_citations = S7::class_list | class_repository_citation | S7::class_character,
    note_uids = S7::class_character,
    notes = S7::class_list | class_note | S7::class_character,
    media_links = S7::class_list | class_media_link | S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s SOUR", self@prim_uid),
          sprintf("1 RESN %s", self@restrictions),
          rep("1 DATA", length(self@facts_recorded) + length(self@responsible_agency) + 
                length(self@data_notes) + length(self@data_note_uids) > 0),
          obj_to_ged(self@facts_recorded, "EVEN") |> increase_level(by = 2),
          sprintf("2 AGNC %s", self@responsible_agency),
          sprintf("2 SNOTE %s", self@data_note_uids),
          obj_to_ged(self@data_notes, "NOTE") |> increase_level(by = 2),
          sprintf("1 AUTH %s", self@originator),
          sprintf("1 TITL %s", self@full_title),
          sprintf("1 ABBR %s", self@short_title),
          sprintf("1 PUBL %s", self@publication_facts),
          obj_to_ged(self@source_text) |> increase_level(by = 1),
          obj_to_ged(self@repo_citations, "REPO") |> increase_level(by = 1),
          self@ids |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_uids),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          obj_to_ged(self@media_links, "OBJE") |> increase_level(by = 1),
          obj_to_ged(self@updated) |> increase_level(by = 1),
          obj_to_ged(self@created) |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@responsible_agency, "@responsible_agency", 0, 1, 1),
      chk_input_size(self@originator, "@originator", 0, 1, 1),
      chk_input_size(self@full_title, "@full_title", 0, 1, 1),
      chk_input_size(self@short_title, "@short_title", 0, 1, 1),
      chk_input_size(self@publication_facts, "@publication_facts", 0, 1, 1),
      chk_input_size(self@source_text, "@source_text", 0, 1),
      chk_input_pattern(self@data_note_uids, "@data_note_uids", reg_uuid(TRUE)),
      chk_input_pattern(self@note_uids, "@note_uids", reg_uuid(TRUE)),
      chk_input_S7classes(self@facts_recorded, "@facts_recorded", class_facts_recorded),
      chk_input_S7classes(self@data_notes, "@data_notes", class_note, ".+"),
      chk_input_S7classes(self@repo_citations, "@repo_citations", class_repository_citation),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+"),
      chk_input_S7classes(self@media_links, "@media_links", class_media_link, reg_uuid(TRUE))
    )
  })


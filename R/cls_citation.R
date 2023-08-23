#' @include cls_validators.R
NULL

#' Create a source citation object
#' 
#' @details 
#' 
#' @inheritParams prop_definitions
#' @return An S7 object representing a GEDCOM SOURCE_CITATION.
#' @export
#' @include cls_date.R cls_translation.R cls_media_link.R cls_note.R
#' @tests
#' expect_snapshot_value(class_citation()@as_ged, "json2")
#' expect_error(class_citation("@1@",
#'                             event_phrase = "phrase"),
#'              regexp = "@event_phrase requires a @event_types")
#' expect_error(class_citation("@1@",
#'                             role = "HUSB"),
#'              regexp = "@role requires a @event_types")
#' expect_error(class_citation("@1@",
#'                             event_types = "BIRT", role_phrase = "phrase"),
#'              regexp = "@role_phrase requires a @role")
#' expect_error(class_citation("@1@",
#'                             certainty = "4"),
#'              regexp = "@certainty has an invalid value")
#' expect_error(class_citation("@1@",
#'                             notes = ""),
#'              regexp = "@notes is in an invalid format")
#' expect_snapshot_value(class_citation("@1@",
#'                                      where = "page 2",
#'                                      date = "2 JUN 2006",
#'                                      source_text = c("verbatim","text"),
#'                                      event_types = "BIRT, MARR, DEAT",
#'                                      event_phrase = "and others",
#'                                      role = "HUSB",
#'                                      role_phrase = "phrase",
#'                                      certainty = "3",
#'                                      media_links = class_media_link("@34E@"),
#'                                      note_xrefs = c("@WER@",
#'                                                    "@4334@"),
#'                                      notes = c("these are","some notes"))@as_ged, "json2")
class_citation <- S7::new_class(
  "class_citation",
  package = "gedcomS7",
  properties = list(
    sour_xref = S7::new_property(S7::class_character, default = "@VOID@"),
    where = S7::class_character,
    date = S7::class_character | class_date_value,
    source_text = S7::class_list | class_translation_txt | S7::class_character,
    event_types = S7::class_character,
    event_phrase = S7::class_character,
    role = S7::class_character,
    role_phrase = S7::class_character,
    certainty = S7::class_character,
    media_links = S7::class_list | class_media_link | S7::class_character,
    note_xrefs = S7::class_character,
    notes = S7::class_list | class_note | S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 SOUR %s", self@sour_xref),
          sprintf("1 PAGE %s", self@where),
          rep("1 DATA", length(self@date) + 
                length(self@source_text) > 0),
          obj_to_ged(self@date, "DATE") |> increase_level(by = 2),
          obj_to_ged(self@source_text, "TEXT") |> increase_level(by = 2),
          sprintf("1 EVEN %s", self@event_types),
          sprintf("2 PHRASE %s", self@event_phrase),
          sprintf("2 ROLE %s", self@role),
          sprintf("3 PHRASE %s", self@role_phrase),
          sprintf("1 QUAY %s", self@certainty),
          obj_to_ged(self@media_links, "OBJE") |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1)
        ) 
      })
  ),
  
  validator = function(self){
    c(
      chk_input_size(self@sour_xref, "@sour_xref", 1, 1),
      chk_input_size(self@where, "@where", 0, 1, 1),
      chk_input_size(self@date, "@date", 0, 1),
      chk_input_size(self@event_types, "@event_types", 0, 1),
      chk_input_size(self@event_phrase, "@event_phrase", 0, 1, 1),
      chk_input_size(self@role, "@role", 0, 1),
      chk_input_size(self@role_phrase, "@role_phrase", 0, 1, 1),
      chk_input_size(self@certainty, "@certainty", 0, 1),
      chk_input_parents(self@event_phrase, "@event_phrase", 
                        self@event_types, "@event_types"),
      chk_input_parents(self@role, "@role", 
                        self@event_types, "@event_types"),
      chk_input_parents(self@role_phrase, "@role_phrase", 
                        self@role, "@role"),
      chk_input_pattern(self@sour_xref, "@sour_xref", reg_xref(TRUE)),
      chk_input_pattern(self@note_xrefs, "@note_xrefs", reg_xref(TRUE)),
    #  chk_input_choice(self@event_types, "@event_types", val_fact_types()), it's a comma separated value
      chk_input_choice(self@role, "@role", val_roles()),
      chk_input_choice(self@certainty, "@certainty", val_certainty()),
      chk_input_S7classes(self@source_text, "@source_text", class_translation_txt, ".+"),
      chk_input_S7classes(self@media_links, "@media_links", class_media_link, reg_xref(TRUE)),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+")
    )
  }
  
)


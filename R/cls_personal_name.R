#' @include cls_validators.R
NULL

#' @export
class_name_pieces <- S7::new_class(
  "class_name_pieces",
  package = "gedcomS7",
  properties = list(
    prefix = S7::class_character,
    given = S7::class_character,
    nickname = S7::class_character,
    surname_prefix = S7::class_character,
    surname = S7::class_character,
    suffix = S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 NPFX %s", self@prefix),
          sprintf("0 GIVN %s", self@given),
          sprintf("0 NICK %s", self@nickname),
          sprintf("0 SPFX %s", self@surname_prefix),
          sprintf("0 SURN %s", self@surname),
          sprintf("0 NSFX %s", self@suffix)
        )
      })
  ),
  validator = function(self) {
    c(
      chk_input_size(self@prefix, "@prefix", min_val = 1),
      chk_input_size(self@given, "@given", min_val = 1),
      chk_input_size(self@nickname,"@nickname", min_val = 1),
      chk_input_size(self@surname_prefix, "@surname_prefix", min_val = 1),
      chk_input_size(self@surname, "@surname", min_val = 1),
      chk_input_size(self@suffix, "@suffix", min_val = 1)
    )
  }
  
)

#' @export
class_personal_name_trans <- S7::new_class(
  "class_personal_name_trans",
  package = "gedcomS7",
  properties = list(
    name = S7::class_character,
    language = S7::class_character,
    name_pieces = NULL | class_name_pieces,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 TRAN %s", self@name),
          sprintf("1 LANG %s", self@language),
          obj_to_ged(self@name_pieces) |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@name, "@name", 1, 1, 1),
      chk_input_size(self@language, "@language", 1, 1),
      #TODO: language option
      chk_input_size(self@name_pieces, "@name_pieces", 0, 1),
    )
  })

#' @export
#' @include cls_note.R cls_citation.R
class_personal_name <- S7::new_class(
  "class_personal_name",
  package = "gedcomS7",
  properties = list(
    name = S7::class_character,
    type = S7::class_character,
    type_phrase = S7::class_character,
    name_pieces = NULL | class_name_pieces,
    name_alts = S7::class_list | class_personal_name_trans,
    notes = S7::class_list | class_note | S7::class_character,
    note_uids = S7::class_character,
    citations = S7::class_list | class_citation | S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 NAME %s", self@name),
          sprintf("1 TYPE %s", self@type),
          sprintf("2 PHRASE %s", rep(self@type_phrase, length(self@type))),
          obj_to_ged(self@name_pieces) |> increase_level(by = 1),
          obj_to_ged(self@name_alts) |> increase_level(by = 1),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_uids),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1)
        )
      })
  ),
  
  validator = function(self){
    c(
      chk_input_size(self@name, "@name", 1, 1, 1),
      chk_input_size(self@type, "@type", 0, 1),
      #TODO: type selection
      chk_input_size(self@type_phrase, "@type_phrase", 0, 1, 1),
      chk_input_size(self@name_pieces, "@name_pieces", 0, 1),
      chk_input_S7classes(self@name_alts, "@name_alts", class_personal_name_trans),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+"),
      chk_input_pattern(self@note_uids, "@note_uids", reg_uuid(TRUE)),
      chk_input_S7classes(self@citations, "@citations", class_citation, reg_uuid(TRUE))
    )
  }
)


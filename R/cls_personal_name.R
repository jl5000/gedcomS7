#' @include cls_validators.R
NULL


#' Create a name pieces object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM PERSONAL_NAME_PIECES.
#' @export
#' @tests
#' expect_snapshot_value(class_name_pieces(prefix = "Mr",
#'                                         given = "Joe",
#'                                         nickname = c("J","Jock"),
#'                                         surname_prefix = "Mc",
#'                                         surname = "Bloggs",
#'                                         suffix = "Jr")@as_ged, "json2")
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

extract_name_info <- function(lines, location){
  
  nts <- find_ged_values(lines, c(location, "NOTE"))
  
  surn <- find_ged_values(lines, c(location, "SURN"))
  full <- find_ged_values(lines, location)
  if(length(surn) == 0){
    surn <- sub("^.*/(.+)/.*$", "\\1", full)
  }
  
  class_name_info(
    full = full,
    type = find_ged_values(lines, c(location, "TYPE")),
    prefix = find_ged_values(lines, c(location, "NPFX")),
    given = find_ged_values(lines, c(location, "GIVN")),
    nickname = find_ged_values(lines, c(location, "NICK")),
    surname_prefix = find_ged_values(lines, c(location, "SPFX")),
    surname = surn,
    suffix = find_ged_values(lines, c(location, "NSFX")),
    note_links = nts[grepl(reg_xref(TRUE), nts)],
    notes = nts[!grepl(reg_xref(TRUE), nts)],
    citations = extract_citations(lines, location)
  )
  
}



#' Create a name translation object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM personal name translation substructure.
#' @export
#' @tests
#' expect_snapshot_value(class_personal_name_tran("Joe /Bloggs/",
#'                                                language = "en")@as_ged, "json2")
#' expect_snapshot_value(class_personal_name_tran("Joe /Bloggs/",
#'                                                language = "en",
#'                                                name_pieces = class_name_pieces(nickname = "JJ"))@as_ged, "json2")
class_personal_name_tran <- S7::new_class(
  "class_personal_name_tran",
  package = "gedcomS7",
  properties = list(
    pers_name = S7::class_character,
    language = S7::class_character,
    name_pieces = NULL | class_name_pieces,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 TRAN %s", self@pers_name),
          sprintf("1 LANG %s", self@language),
          obj_to_ged(self@name_pieces) |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@pers_name, "@pers_name", 1, 1, 1),
      chk_input_size(self@language, "@language", 1, 1),
      #TODO: language option
      chk_input_size(self@name_pieces, "@name_pieces", 0, 1)
    )
  })

#' Create a personal name object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM PERSONAL_NAME_STRUCTURE.
#' @export
#' @include cls_note.R cls_citation.R
#' @tests
#' expect_error(class_personal_name("Joe /Bloggs/", name_type = "birth"),
#'              regexp = "@name_type has an invalid value")
#' expect_error(class_personal_name("Joe /Bloggs/", type_phrase = "After 2012"),
#'              regexp = "@type_phrase requires a @name_type")
#' expect_snapshot_value(class_personal_name("Joe /Bloggs/",
#'                                           name_type = "OTHER",
#'                                           type_phrase = "Circus",
#'                                           name_pieces = class_name_pieces(nickname = "JJ"),
#'                                           name_translations = class_personal_name_tran("Joey /Bloggoni/",
#'                                                                                        language = "it"),
#'                                           notes = "This is a note",
#'                                           note_xrefs = c("@IUY@","@733@"),
#'                                           citations = c("@S1@","@S3@","@S7@"))@as_ged, "json2")
class_personal_name <- S7::new_class(
  "class_personal_name",
  package = "gedcomS7",
  properties = list(
    pers_name = S7::class_character,
    name_type = S7::class_character,
    type_phrase = S7::class_character,
    name_pieces = NULL | class_name_pieces,
    name_translations = S7::class_list | class_personal_name_tran,
    notes = S7::class_list | class_note | S7::class_character,
    note_xrefs = S7::class_character,
    citations = S7::class_list | class_citation | S7::class_character,
    
    as_val = S7::new_property(S7::class_character, 
                              getter = function(self) self@pers_name),
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 NAME %s", self@pers_name),
          sprintf("1 TYPE %s", self@name_type),
          sprintf("2 PHRASE %s", self@type_phrase),
          obj_to_ged(self@name_pieces) |> increase_level(by = 1),
          obj_to_ged(self@name_translations) |> increase_level(by = 1),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1)
        )
      })
  ),
  
  validator = function(self){
    c(
      chk_input_size(self@pers_name, "@pers_name", 1, 1, 1),
      chk_input_size(self@name_type, "@name_type", 0, 1),
      chk_input_choice(self@name_type, "@name_type", val_name_types()),
      chk_input_size(self@type_phrase, "@type_phrase", 0, 1, 1),
      chk_input_parents(self@type_phrase, "@type_phrase", self@name_type, "@name_type"),
      chk_input_size(self@name_pieces, "@name_pieces", 0, 1),
      chk_input_S7classes(self@name_translations, "@name_translations", class_personal_name_tran),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+"),
      chk_input_pattern(self@note_xrefs, "@note_xrefs", reg_xref(TRUE)),
      chk_input_S7classes(self@citations, "@citations", class_citation, reg_xref(TRUE))
    )
  }
)

extract_personal_names <- function(rec_lines){
  
  name_lst <- find_ged_values(rec_lines, "NAME", return_list = TRUE)
  if(length(name_lst) == 0) return(list())
  
  lapply(name_lst, \(x){
    nm <- extract_name_info(x, "NAME")
    phon_lst <- find_ged_values(x, c("NAME","FONE"), return_list = TRUE)
    rom_lst <- find_ged_values(x, c("NAME","ROMN"), return_list = TRUE)
    
    class_personal_name(
      name = nm,
      phon_names = lapply(phon_lst, extract_name_info, "FONE"),
      rom_names = lapply(rom_lst, extract_name_info, "ROMN")
    )
  })
}
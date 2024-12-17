
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
#'                                         suffix = "Jr")@c_as_ged, "json2")
class_name_pieces <- S7::new_class(
  "class_name_pieces",
  package = "gedcomS7",
  properties = list(
    prefix = S7::new_property(S7::class_character,
                              validator = function(value){
                                chk_input_size(value, min_val = 1)
                              }),
    given = S7::new_property(S7::class_character,
                             validator = function(value){
                               chk_input_size(value, min_val = 1)
                             }),
    nickname = S7::new_property(S7::class_character,
                                validator = function(value){
                                  chk_input_size(value, min_val = 1)
                                }),
    surname_prefix = S7::new_property(S7::class_character,
                                      validator = function(value){
                                        chk_input_size(value, min_val = 1)
                                      }),
    surname = S7::new_property(S7::class_character,
                               validator = function(value){
                                 chk_input_size(value, min_val = 1)
                               }),
    suffix = S7::new_property(S7::class_character,
                              validator = function(value){
                                chk_input_size(value, min_val = 1)
                              }),
    
    c_as_ged = S7::new_property(
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
  )
)



#' Create a name translation object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM personal name translation substructure.
#' @export
#' @tests
#' expect_snapshot_value(class_personal_name_tran("Joe /Bloggs/",
#'                                                language = "en")@c_as_ged, "json2")
#' expect_snapshot_value(class_personal_name_tran("Joe /Bloggs/",
#'                                                language = "en",
#'                                                name_pieces = class_name_pieces(nickname = "JJ"))@c_as_ged, "json2")
class_personal_name_tran <- S7::new_class(
  "class_personal_name_tran",
  package = "gedcomS7",
  properties = list(
    pers_name = S7::new_property(S7::class_character,
                                 validator = function(value){
                                   chk_input_size(value, 1, 1, 1)
                                 }),
    language = S7::new_property(S7::class_character,
                                validator = function(value){
                                  c(
                                    chk_input_size(value, 1, 1, 1)
                                    #TODO: language option
                                  )
                                }),
    name_pieces = S7::new_property(NULL | S7::new_S3_class("gedcomS7::class_name_pieces"),
                                   validator = function(value){
                                     chk_input_size(value, 0, 1)
                                   }),
    
    c_as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 TRAN %s", self@pers_name),
          sprintf("1 LANG %s", self@language),
          obj_to_ged(self@name_pieces) |> increase_level(by = 1)
        )
      })
  )
)



#' Create a personal name object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM PERSONAL_NAME_STRUCTURE.
#' @export
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
#'                                           citations = c("@S1@","@S3@","@S7@"))@c_as_ged, "json2")
class_personal_name <- S7::new_class(
  "class_personal_name",
  package = "gedcomS7",
  properties = list(
    pers_name = S7::new_property(S7::class_character,
                                 validator = function(value){
                                   chk_input_size(value, 1, 1, 1)
                                 }),
    name_type = S7::new_property(S7::class_character,
                                 validator = function(value){
                                   c(
                                     chk_input_size(value, 0, 1),
                                     chk_input_choice(value, val_name_types())
                                   )
                                 }),
    type_phrase = S7::new_property(S7::class_character,
                                   validator = function(value){
                                     chk_input_size(value, 0, 1, 1)
                                   }),
    name_pieces = S7::new_property(NULL | S7::new_S3_class("gedcomS7::class_name_pieces"),
                                   validator = function(value){
                                     chk_input_size(value, 0, 1)
                                   }),
    name_translations = S7::new_property(S7::class_list | 
                                           S7::new_S3_class("gedcomS7::class_personal_name_tran"),
                                         validator = function(value){
                                           chk_input_S7classes(value, class_personal_name_tran)
                                         }),
    notes = S7::new_property(S7::class_list | 
                               S7::new_S3_class("gedcomS7::class_note") | 
                               S7::class_character,
                             validator = function(value){
                               chk_input_S7classes(value, class_note, ".+")
                             }),
    note_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    citations = S7::new_property(S7::class_list | class_citation | S7::class_character,
                                 validator = function(value){
                                   chk_input_S7classes(value, class_citation, reg_xref(TRUE))
                                 }),
    
    c_as_val = S7::new_property(S7::class_character, 
                              getter = function(self) self@pers_name),
    
    c_as_ged = S7::new_property(
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
    chk_input_parents(self@type_phrase, "@type_phrase", self@name_type, "@name_type")
  }
)

parse_name_pieces <- function(lines, location = NULL){
  class_name_pieces(
    prefix = find_ged_values(lines, c(location, "NPFX")),
    given = find_ged_values(lines, c(location, "GIVN")),
    nickname = find_ged_values(lines, c(location, "NICK")),
    surname_prefix = find_ged_values(lines, c(location, "SPFX")),
    surname = find_ged_values(lines, c(location, "SURN")),
    suffix = find_ged_values(lines, c(location, "NSFX"))
  )
}

parse_personal_name_tran <- function(lines, location = NULL){
  tran_lst <- find_ged_values(lines, c(location, "TRAN"), return_list = TRUE)
  if(length(tran_lst) == 0) return(list())
  
  lapply(tran_lst, \(x){
    class_personal_name_tran(
      pers_name = find_ged_values(x, "TRAN"),
      language = find_ged_values(x, c("TRAN","LANG")),
      name_pieces = parse_name_pieces(x, "TRAN")
    )
  })
  
}

parse_personal_names <- function(rec_lines){
  
  name_lst <- find_ged_values(rec_lines, "NAME", return_list = TRUE)
  if(length(name_lst) == 0) return(list())
  
  lapply(name_lst, \(x){
    class_personal_name(
      pers_name = find_ged_values(x, "NAME"),
      name_type = find_ged_values(x, c("NAME","TYPE")),
      type_phrase = find_ged_values(x, c("NAME","TYPE","PHRASE")),
      name_pieces = parse_name_pieces(x, "NAME"),
      name_translations = parse_personal_name_tran(x, "NAME"),
      notes = parse_notes(x, "NAME"),
      note_xrefs = find_ged_values(x, c("NAME","SNOTE")),
      citations = parse_citations(x, "NAME")
    )
  })
}

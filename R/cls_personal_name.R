
#' Create a name pieces object
#' 
#' @param prefix The name prefix, e.g. Cmdr.
#' @param given The given name or earned name.
#' @param nickname A descriptive or familiar name that is used instead of, or in addition to, one’s proper
#' name.
#' @param surname_prefix Surname prefix or article used in a family name. 
#' For example in the name "de la Cruz", this value would be "de la".
#' @param surname Surname or family name.
#' @param suffix Name piece that appears after the given name and surname parts, e.g. Jr.
#'
#' @returns An S7 object representing a GEDCOM PERSONAL_NAME_PIECES.
#' @export
#' @tests
#' expect_snapshot_value(PersonalNamePieces(prefix = "Mr",
#'                                         given = "Joe",
#'                                         nickname = c("J","Jock"),
#'                                         surname_prefix = "Mc",
#'                                         surname = "Bloggs",
#'                                         suffix = "Jr")@GEDCOM, "json2")
PersonalNamePieces <- S7::new_class(
  "PersonalNamePieces",
  parent = GedcomS7class,
  properties = list(
    prefix = prop_char(min_char = 1),
    given = prop_char(min_char = 1),
    nickname = prop_char(min_char = 1),
    surname_prefix = prop_char(min_char = 1),
    surname = prop_char(min_char = 1),
    suffix = prop_char(min_char = 1),
    
    GEDCOM = S7::new_property(
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
#' 
#' @returns An S7 object representing a GEDCOM personal name translation substructure.
#' @export
#' @tests
#' expect_snapshot_value(PersonalNameTran("Joe /Bloggs/",
#'                                                language = "en")@GEDCOM, "json2")
#' expect_snapshot_value(PersonalNameTran("Joe /Bloggs/",
#'                                                language = "en",
#'                                                name_pieces = PersonalNamePieces(nickname = "JJ"))@GEDCOM, "json2")
PersonalNameTran <- S7::new_class(
  "PersonalNameTran",
  parent = GedcomS7class,
  properties = list(
    pers_name = prop_char(1, 1, 1),
    language = prop_char(1, 1, 1),
    name_pieces = prop_S7obj("name_pieces", PersonalNamePieces),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 TRAN %s", self@pers_name),
          sprintf("1 LANG %s", self@language),
          as_ged(self@name_pieces) |> level_up(1)
        )
      })
  )
)



#' Create a personal name object
#' 
#' @inheritParams prop_definitions 
#' @param name_type An optional name type taken from `val_name_types()`.
#' @param type_phrase An optional free text description of the name type. This
#' is required if the name type is "OTHER".
#' @param name_translations A `PersonalNameTran()` object or a list of them, providing
#' different translations of this personal name.
#' 
#' @returns An S7 object representing a GEDCOM PERSONAL_NAME_STRUCTURE.
#' @export
#' @tests
#' expect_warning(PersonalName("Joe Bloggs"),
#'                regexp = "Did you forget to enclose the surname in forward slashes")
#' expect_error(PersonalName("Joe /Bloggs/", name_type = "birth"),
#'              regexp = "@name_type has an invalid value")
#' expect_error(PersonalName("Joe /Bloggs/", type_phrase = "After 2012"),
#'              regexp = "@type_phrase requires a @name_type")
#' expect_error(PersonalName("Joe /Bloggs/", name_type = "OTHER"),
#'              regexp = "A @type_phrase must be given if @name_type is 'OTHER'")
#' expect_snapshot_value(PersonalName("Joe /Bloggs/",
#'                                           name_type = "OTHER",
#'                                           type_phrase = "Circus",
#'                                           name_pieces = PersonalNamePieces(nickname = "JJ"),
#'                                           name_translations = PersonalNameTran("Joey /Bloggoni/",
#'                                                                                        language = "it"),
#'                                           notes = "This is a note",
#'                                           note_xrefs = c("@IUY@","@733@"),
#'                                           citations = c("@S1@","@S3@","@S7@"))@GEDCOM, "json2")
PersonalName <- S7::new_class(
  "PersonalName",
  parent = GedcomS7class,
  properties = list(
    pers_name = prop_char(1, 1, 1),
    name_type = prop_char(0, 1, choices = val_name_types()),
    type_phrase = prop_char(0, 1, 1),
    name_pieces = prop_S7obj("name_pieces", PersonalNamePieces),
    name_translations = prop_S7list("name_translations", PersonalNameTran),
    notes = prop_S7list("notes", Note),
    note_xrefs = prop_char(pattern = reg_xref(TRUE)),
    citations = prop_S7list("citations", SourceCitation),
    
    GEDCOM_STRING = S7::new_property(S7::class_character, 
                              getter = function(self) self@pers_name),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 NAME %s", self@pers_name),
          sprintf("1 TYPE %s", self@name_type),
          sprintf("2 PHRASE %s", self@type_phrase),
          as_ged(self@name_pieces) |> level_up(1),
          as_ged(self@name_translations) |> level_up(1),
          notes_to_ged(self@notes, self@note_xrefs) |> level_up(1),
          as_ged(self@citations) |> level_up(1)
        )
      })
  ),
  
  validator = function(self){
    if(!grepl("/", self@pers_name))
      warning("Did you forget to enclose the surname in forward slashes?: ", self@pers_name)
    
    c(
      chk_input_phrase(self@type_phrase, "@type_phrase",
                       self@name_type, "@name_type", "OTHER"),
      chk_input_parents(self@type_phrase, "@type_phrase", self@name_type, "@name_type")
    )
  }
)

parse_name_pieces <- function(lines, location = NULL){
  PersonalNamePieces(
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

  lapply(tran_lst, \(x){
    PersonalNameTran(
      pers_name = find_ged_values(x, "TRAN"),
      language = find_ged_values(x, c("TRAN","LANG")),
      name_pieces = parse_name_pieces(x, "TRAN")
    )
  })
  
}

parse_personal_names <- function(rec_lines){
  
  name_lst <- find_ged_values(rec_lines, "NAME", return_list = TRUE)

  lapply(name_lst, \(x){
    PersonalName(
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

S7::method(summary, PersonalName) <- function(object, ...){
  exdent <- 17
  to_console("Personal Name:", object@pers_name, exdent)
  to_console_value_with_phrase("Name Type:", 
                               object@name_type, object@type_phrase, 
                               exdent)
  cat("\n")
  to_console("Translations:", length(object@name_translations), exdent)
  to_console("Citations:", length(object@citations), exdent)
  to_console("Notes:", length(object@notes) + length(object@note_xrefs), exdent)
}

S7::method(summary, PersonalNameTran) <- function(object, ...){
  exdent <- 17
  to_console("Translated Name:", object@pers_name, exdent)
  to_console("Language:", object@language, exdent)
}

S7::method(summary, PersonalNamePieces) <- function(object, ...){
  exdent <- 17
  to_console("Prefix:", object@prefix, exdent)
  to_console("Given:", object@given, exdent)
  to_console("Nickname:", object@nickname, exdent)
  to_console("Surname Prefix:", object@surname_prefix, exdent)
  to_console("Surname:", object@surname, exdent)
  to_console("Suffix:", object@suffix, exdent)
}


#' Define properties in GEDCOM 7.0 specification
#' 
#' Define common properties used in S7 classes.
#' 
#' @details This empty function serves as a single location where all shared properties are defined, 
#' mainly for efficiency and maintainability.
#' @name prop_definitions
#' 
#' @param text A character string. New lines are created with \\n.
#' @param language A character string of language tags as defined in BCP 47.
#' 
#' @param media_type The media type as defined in RFC 2045.
#' @param translations A `TranslationText()` object or a list of them.
#' One for each alternate translation of the text.
#' @param title The title of the multimedia record.
#' 
#' @param source_text A verbatim copy of any description contained within the source. This can 
#' either be a `TranslationText()` object, a list of them, or a character vector of text. 
#' @param fact_type A code indicating the type of fact. This must be taken from one of
#' `val_individual_event_types()`, `val_individual_attribute_types()`,
#' `val_family_event_types()`, or `val_family_attribute_types()`. A generic event ("EVEN")
#' or attribute ("FACT") can also be defined for more bespoke facts. See `fact_rules_df()`
#' for the set of rules surrounding the codes allowed.
#' @param fact_val A value associated with the fact. For example for "NCHI" this would
#' be the number of children. See `fact_rules_df()` for the set of rules surrounding the 
#' need for values and the values allowed.
#' @param fact_desc A further classification of the fact. This is required for generic
#' events or attributes. See `fact_rules_df()` for the set of rules surrounding the 
#' need for this.
#' @param medium A value from `val_medium_types()`. If "OTHER" is selected then a `@medium_phrase`
#' must be given.
#' @param medium_phrase A free text description of the medium. This is mandatory if `@medium` is
#' "OTHER".
#' @param media_alt A named vector of the media in alternative media forms, c(form = location)
#' @param pers_name The full name of the individual. Surnames should be enclosed in forward slashes.
#' @param name_pieces A `PersonalNamePieces()` object defining the pieces of the full name.
#' 
#' @param place The associated place. This can either be a 
#' `Place()` object or a character string (a comma-separated string of region names, 
#' ordered from smallest to largest).
#' @param address The address given either as a `Address()` object or as a character string.
#' This would be as written on a mailing label with new lines separated by \\n.
#' @param phone_numbers A character vector of phone numbers.
#' @param emails A character vector of email addresses.
#' @param faxes A character vector of fax numbers.
#' @param web_pages A character vector of web page URLs.
#' @param agency The organization, institution, corporation, person, or other entity that 
#' has responsibility for the associated fact. Examples are an employer of a person of an 
#' associated occupation, or an educational establishment that has awarded a scholastic award.
#' @param relig_affil A religious denomination associated with the fact.
#' @param cause Used in special cases to record the reasons which precipitated the fact (e.g. cause of death). 
#' @param confidential A logical value indicating whether the associated record/fact should be
#' treated as confidential. This allows them to be excluded on export.
#' @param locked A logical value indicating whether the associated record/fact should be
#' treated as read-only.
#' @param private A logical value indicating whether the associated record/fact should be
#' treated as private. This allows them to be excluded on export.
#' @param associations Associated individuals. This can either be a `Association()` object or a list of them.
#' @param age A character string that indicates the age in years, months, weeks and/or days 
#' that the individual was at the time of the fact. Any combination of these is permitted. 
#' Any labels must come after their corresponding number, for example; "4y 8m 1w 3d". 
#' Age bounds can also be included, for example; "< 40y". If the age doesn't fit this format then describe the age 
#' in the corresponding phrase parameter.
#' @param husb_age A character string that indicates the age in years, months, weeks and/or days 
#' that the husband was at the time of the fact. Any combination of these is permitted. 
#' Any labels must come after their corresponding number, for example; "4y 8m 1w 3d". 
#' Age bounds can also be included, for example; "< 40y". If the age doesn't fit this format then describe the age 
#' in the corresponding phrase parameter.
#' @param wife_age A character string that indicates the age in years, months, weeks and/or days 
#' that the wife was at the time of the fact. Any combination of these is permitted. 
#' Any labels must come after their corresponding number, for example; "4y 8m 1w 3d". 
#' Age bounds can also be included, for example; "< 40y". If the age doesn't fit this format then describe the age 
#' in the corresponding phrase parameter.
#' @param age_phrase Free text information that cannot be expressed in the individual's age.
#' @param husb_age_phrase Free text information that cannot be expressed in the husband's age.
#' @param wife_age_phrase Free text information that cannot be expressed in the wife's age.
#' 
#' @param temple_name The name of a temple of The Church of Jesus Christ of Latter-day Saints.
#' @param ord_state An optional value from `val_ordinance_states(@ord_type)`. 
#' @param state_date The ordinance date given either as a formatted GEDCOM string, or a
#' `DateExact` object.
#' @param state_time The ordinance time given either as a formatted GEDCOM string, or a
#' `Time` object.
#' 
#' @param XREF The cross-reference identifier for this record. You should not edit this at all
#' as maintenance of these is done automatically.
#' 
#' @param fam_xref The cross-reference identifier of a family record.
#' 
#' @param note_xrefs A character vector of relevant note record cross-reference identifiers.
#' @param subm_xrefs A character vector of relevant submitter record cross-reference identifiers.
#' 
#' @param unique_ids A character vector of enduring and globally-unique identifiers. These need
#' to be formatted in line with RFC 4122 and can be generated with `uuid::UUIDgenerate()`.
#' @param user_ids A character vector of user-generated identifiers. The type of the identifiers can
#' be given in the vector names, e.g. c("Driving license number" = "ABC123")
#' @param ext_ids A named character vector of identifiers maintained by an external authority.
#' The names must be given as a URI. See the GEDCOM specification for more information.
#' 
#' 
#' @param media_links Associated multimedia. This can either be a `MediaLink()` object, a list of them,
#' or a character vector of XREFs of multimedia records.
#' @param notes Associated notes. This can either be a `Note()` object, a list of them,
#' or a character vector of notes.
#' @param citations Associated sources. This can either be a `SourceCitation()` object, a list of them,
#' or a character vector of XREFs of source records.
#' 
#' @param year The year given as an integer (greater than 0).
#' @param month The month of the year given as an integer between 1 and 12.
#' @param day The day of the month given as an integer between 1 and 31.
#' 
#' @param date_exact An exact date given either as a formatted GEDCOM string, or a
#' `DateExact()` object. If not given, it will default to today's date.
#' @param date_period A date period given either as a formatted GEDCOM string, or a
#' `DatePeriod()` object.
#' @param date The date given either as a formatted GEDCOM string, or a
#' `DateValue()` object.
#' @param date_sort The date given either as a formatted GEDCOM string, or a
#' `DateSorting()` object.
#' @param date_phrase Textual information that cannot be expressed in the date.
#' @param start_date The start of the period/range given either as a formatted GEDCOM string, or a
#' `DateGregorian()` object.
#' @param end_date The end of the period/range given either as a formatted GEDCOM string, or a
#' `DateGregorian()` object.
#' @param time The time given either as a formatted GEDCOM string, or a
#' `Time()` object.
#' @param created A `CreationDate()` object containing the date the record was created.
#' Creating an object with no parameters sets the date to today.
#' @param updated A `ChangeDate()` object containing the date the record was updated.
#' Creating an object with no parameters sets the date to today.
#' 
#' 
#' @keywords internal
NULL

#' Create a property for a list of gedcomS7 objects
#'
#' Constructs a property that stores a list of objects of a specified gedcomS7 class.
#' 
#' @details
#' If any elements of the list are of type character, then they will automatically
#' be converted into objects of the appropriate class. Any elements that cannot
#' be converted will result in the validator throwing an error. 
#'
#' @param prop_name A character string specifying the name of the property.
#' @param S7_class The gedcomS7 class that each element of the list should conform to.
#'
#' @returns An S7 property object.
#' @keywords internal
prop_S7list <- function(prop_name, S7_class){
  S7::new_property(S7::class_list,
                   getter = function(self) S7::prop(self, prop_name),
                   setter = function(self, value){
                     S7::prop(self, prop_name) <- as.S7class_list(value, S7_class)
                     self
                   },
                   validator = function(value){
                     for(inp in value) if(is.character(inp)) return(inp)
                   })
}

#' Create a property for a single gedcomS7 object
#'
#' Constructs a property that can store a single object of a specified gedcomS7 class.
#' 
#' @details
#' The property will default to a value of NULL if no object is provided. 
#'
#' @param prop_name A character string specifying the name of the property.
#' @param S7_class The gedcomS7 class that the property value should conform to.
#'
#' @returns An S7 property object.
#' @keywords internal
prop_S7obj <- function(prop_name, S7_class){
  # S3 class used so that order of class definition does not matter and classes
  # can be recursive
  S7::new_property(NULL | S7::new_S3_class(paste0("gedcomS7::", deparse(substitute(S7_class)))),
                   getter = function(self) S7::prop(self, prop_name),
                   setter = function(self, value){
                     S7::prop(self, prop_name) <- as.S7class(value, S7_class)
                     self
                   })
}


#' Create a character-based property
#'
#' Constructs a character property with optional constraints on size, 
#' character length, pattern matching, and allowed choices.
#'
#' @param min_size Minimum number of elements allowed.
#' @param max_size Maximum number of elements allowed.
#' @param min_char Minimum number of characters per element.
#' @param max_char Maximum number of characters per element.
#' @param choices Character vector of allowed values.
#' @param pattern Regular expression pattern that values must match.
#' @param names_required Logical indicating whether property values should have names.
#' @param default Default value for the property.
#' @param casting_name The name of the property if you want to explicitly cast it
#' to character type when being set (this allows users to provide other atomic types).
#' @param S7class_names Character vector of gedcomS7 class names that this property
#' could also take.
#'
#' @returns An S7 property object.
#' @keywords internal
prop_char <- function(min_size = NULL, 
                      max_size = NULL, 
                      min_char = NULL, 
                      max_char = NULL,
                      choices = NULL,
                      pattern = NULL,
                      names_required = FALSE,
                      default = NULL,
                      casting_name = NULL,
                      S7class_names = NULL){
  
  classes <- S7::class_character
  for(cls in S7class_names){
    classes <- classes |
      S7::new_S3_class(paste0("gedcomS7::", cls))
  }
  
  getter_fn <- setter_fn <- NULL
  if(!is.null(casting_name)){
    getter_fn <- function(self) S7::prop(self, casting_name)
    setter_fn <- function(self, value){
      S7::prop(self, casting_name) <- as.character(value)
      self
    }
  }
    
  S7::new_property(classes, default = default,
                   getter = getter_fn,
                   setter = setter_fn,
                   validator = function(value){
                     names_test <- NULL
                     if(names_required){
                       names_test <- chk_input_size(names(value), length(value), length(value), 1)
                     }
                       
                     c(
                       chk_input_size(value, min_size, max_size, min_char, max_char),
                       chk_input_choice(value, choices),
                       chk_input_pattern(value, pattern),
                       names_test
                     )
                   })
}

#' Create a numeric property restricted to whole numbers
#'
#' Constructs a numeric property with validation for whole numbers and optional 
#' constraints on size and value range.
#'
#' @param min_size Minimum number of elements allowed.
#' @param max_size Maximum number of elements allowed.
#' @param min_val Minimum numeric value allowed.
#' @param max_val Maximum numeric value allowed.
#'
#' @returns An S7 property object.
#' @keywords internal
prop_whole <- function(min_size = NULL, max_size = NULL, min_val = NULL, max_val = NULL){
  S7::new_property(S7::class_numeric,
                   validator = function(value){
                     c(
                       chk_input_size(value, min_size, max_size, min_val, max_val),
                       chk_whole_number(value)
                     )
                   })
}

#' Create a boolean property
#'
#' Constructs a logical property with a default value and validation to ensure 
#' it contains exactly one element.
#'
#' @param default Logical value to use as the default.
#'
#' @returns An S7 property object.
#' @keywords internal
prop_bool <- function(default = FALSE){
  S7::new_property(S7::class_logical, default = default,
                   validator = function(value){
                     chk_input_size(value, 1, 1)
                   })
}

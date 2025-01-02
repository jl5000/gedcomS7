
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
#' If any `TranslationText()` objects are provided, then you will be forced to define an
#' associated language or media type.
#' @param fact_type TODO
#' 
#' @param medium TODO
#' @param medium_phrase TODO
#' @param media_alt A named vector of the media in alternative media forms, c(form = location)
#' @param pers_name TODO
#' @param name_pieces A `PersonalNamePieces()` object defining the pieces of the full name.
#' 
#' 
#' @param event_type TODO
#' @param fact_val TODO
#' @param fact_desc TODO
#' @param place see territory
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
#' @param xref The cross-reference identifier for this record. You should not edit this at all
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
#' or a character vector of XREFs of multimedia records. If a character vector is provided then only the XREFs themselves
#' can be recorded (and not associated information). This option is easier if 
#' associated information is not required. 
#' @param notes Associated notes. This can either be a `Note()` object, a list of them,
#' or a character vector of notes. If a character vector is provided then only the notes themselves
#' can be recorded (and not associated information). This option is easier if 
#' associated information is not required. 
#' @param citations Associated sources. This can either be a `SourceCitation()` object, a list of them,
#' or a character vector of XREFs of source records. If a character vector is provided then only the XREFs themselves
#' can be recorded (and not associated information). This option is easier if 
#' associated information is not required. 
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

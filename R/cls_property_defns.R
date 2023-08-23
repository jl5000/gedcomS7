
#' Define properties in GEDCOM 7.0 specification
#' 
#' Define all properties used in S7 classes.
#' 
#' @details This empty function serves as a single location where all properties are defined, 
#' mainly for efficiency and maintainability.
#' @name prop_definitions
#' 
#' @param full A full address as it would appear on a mailing label, with lines separated
#' by \n. For example:
#' "The White House\n1600 Pennsylvania Avenue, N.W.\nWashington, DC 20500\nUnited States of America"
#' @param city The city component of the address.
#' @param state The state component of the address.
#' @param postal_code The postal code component of the address.
#' @param country The country component of the address.
#' @param text A character string. New lines are created with \n.
#' @param language Language tags as defined in BCP 47.
#' @param media_type The media type as defined in RFC 2045.
#' @param alt_text A `class_translation_txt` object or a list of them.
#' One for each alternate translation of the text.
#' @param media_xref The cross-reference identifier of a multimedia record.
#' @param title The title of the multimedia record. This will supersede any title given
#' in the record.
#' @param crop Whether to crop the multimedia to a specific area.
#' @param top The number of pixels to omit from the top side of the image.
#' @param left The number of pixels to omit from the left side of the image.
#' @param height The height in pixels of the cropped region.
#' @param width The width in pixels of the cropped region.
#' @param hour The hour of the day given as an integer between 0 and 23.
#' @param minute The minute of the hour given as an integer between 0 and 59.
#' @param second The second of the minute given as an integer between 0 and 59.
#' @param fraction The fraction of the second given as 
#' an integer.
#' @param utc Whether the time is in Coordinated Universal Time (UTC) (TRUE, the default) or
#' is in local time (FALSE).
#' @param year The year given as an integer (greater than 0).
#' @param month The month of the year given as an integer between 1 and 12.
#' @param day The day of the month given as an integer between 1 and 31.
#' @param bce Whether the date is Before the Common Era. This is FALSE by default,
#' but if TRUE, only the year should be given.
#' @param date_greg A Gregorian date given either as a formatted GEDCOM string, or a
#' `class_date_greg` object.
#' @param about Whether the date is near to the date given.
#' @param calc Whether the date is calculated from other values.
#' @param est Whether the date is near to the date given, and is calculated from other values.
#' @param start_date The start of the period/range given either as a formatted GEDCOM string, or a
#' `class_date_greg` object.
#' @param end_date The end of the period/range given either as a formatted GEDCOM string, or a
#' `class_date_greg` object.
#' @param date The date given either as a formatted GEDCOM string, or a
#' `class_date_value` object.
#' @param date_phrase Textual information that cannot be expressed in the date.
#' @param time The time given either as a formatted GEDCOM string, or a
#' `class_time` object.
#' @param sour_xref The cross-reference identifier of a source record.
#' @param where A specific location within the information referenced. For a published work, this could
#' include the volume of a multi-volume work and the page number or numbers. For a
#' periodical, it could include volume, issue, and page numbers. For a newspaper, it could
#' include a date, page number, and column number. For an unpublished source or micro‐
#' filmed works, this could be a film or sheet number, page number, or frame number. A
#' census record might have an enumerating district, page number, line number, dwelling
#' number, and family number.
#' It is recommended that the data in this field be formatted comma-separated with label:
#'   value pairs
#' @param source_text A verbatim copy of any description contained within the source. This can 
#' either be a `class_translation_txt` object, a list of them, or a character vector of text. 
#' If any `class_translation_txt` objects are provided, then you will be forced to define an
#' associated language or media type.
#' @param event_types A character string indicating the types
#' of events that were recorded in a particular source. Each event type is separated by a
#' comma and space. For example, a parish register of births, deaths, and marriages
#' would be BIRT, DEAT, MARR.
#' @param event_phrase Textual information that cannot be expressed in the event types.
#' @param role What role this person played in this event.
#' @param role_phrase Textual information that cannot be expressed in the role.
#' @param certainty An enumerated value indicating the credibility of a
#' piece of information, based on its supporting evidence. Some systems use this feature to
#' rank multiple conflicting opinions for display of most likely information first. It is not
#' intended to eliminate the receivers’ need to evaluate the evidence for themselves.
#' "0" = unreliable/estimated data
#' "1" = Questionable reliability of evidence 
#' "2" = Secondary evidence, data officially recorded sometime after event
#' "3" = Direct and primary evidence used, or by dominance of the evidence
#' @param media_links Associated multimedia. This can either be a `class_media_link` object, a list of them,
#' or a character vector of XREFs of multimedia records. If a character vector is provided then only the XREFs themselves
#' can be recorded (and not associated information). This option is easier if 
#' associated information is not required. 
#' @param note_xrefs A character vector of relevant note record cross-reference identifiers.
#' @param notes Associated notes. This can either be a `class_note` object, a list of them,
#' or a character vector of notes. If a character vector is provided then only the notes themselves
#' can be recorded (and not associated information). This option is easier if 
#' associated information is not required. 
NULL
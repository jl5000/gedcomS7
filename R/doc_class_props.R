
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
#' @param languages
#' @param media_type The media type as defined in RFC 2045.
#' @param translations A `class_translation_txt` object or a list of them.
#' One for each alternate translation of the text.
#' @param title The title of the multimedia record. This will supersede any title given
#' in the record.
#' @param crop Whether to crop the multimedia to a specific area.
#' @param top The number of pixels to omit from the top side of the image.
#' @param left The number of pixels to omit from the left side of the image.
#' @param height The height in pixels of the cropped region.
#' @param width The width in pixels of the cropped region.
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
#' @param fact_type 
#' @param fact_phrase Textual information that cannot be expressed in the fact type.
#' @param role What role this person played in this fact.
#' @param role_phrase Textual information that cannot be expressed in the role.
#' @param certainty An enumerated value indicating the credibility of a
#' piece of information, based on its supporting evidence. Some systems use this feature to
#' rank multiple conflicting opinions for display of most likely information first. It is not
#' intended to eliminate the receivers’ need to evaluate the evidence for themselves.
#' "0" = unreliable/estimated data
#' "1" = Questionable reliability of evidence 
#' "2" = Secondary evidence, data officially recorded sometime after event
#' "3" = Direct and primary evidence used, or by dominance of the evidence
#' @param indi_phrase Textual information that cannot be expressed in the @indi_xref.
#' @param relation_is
#' @param relation_phrase
#' @param place_name
#' @param place_form
#' @param place_translations
#' @param lat_long
#' @param ext_ids
#' @param fact_types A character string indicating the types
#' of events that were recorded in a particular source. Each event type is separated by a
#' comma and space. For example, a parish register of births, deaths, and marriages
#' would be BIRT, DEAT, MARR. 
#' @param territory
#' @param pedigree
#' @param pedigree_phrase
#' @param confidence
#' @param confidence_phrase
#' @param call_numbers
#' @param location
#' @param medium
#' @param medium_phrase
#' @param media_alt A named vector of the media in alternative media forms, c(form = location)
#' @param prefix
#' @param given
#' @param nickname
#' @param surname_prefix
#' @param surname
#' @param suffix
#' @param pers_name
#' @param name_pieces
#' @param pers_name
#' @param name_type
#' @param type_phrase
#' @param name_translations
#' @param event_type
#' @param fact_val
#' @param fact_desc
#' @param place see territory
#' @param address The address given either as a `class_address` object or as a character string.
#' This would be as written on a mailing label with new lines separated by \n.
#' @param phone_numbers 
#' @param emails
#' @param faxes
#' @param web_pages
#' @param agency
#' @param relig_affil
#' @param cause
#' @param confidential
#' @param locked
#' @param private
#' @param associations
#' @param age
#' @param age_phrase
#' @param husb_age
#' @param husb_age_phrase
#' @param wife_age
#' @param wife_age_phrase
#' @param adop_parent
#' @param adop_parent_phrase
#' @param pers_names
#' @param sex
#' @param fam_links_chil
#' @param fam_links_spou
#' @param files
#' @param repo_name
#' @param facts_recorded
#' @param originator
#' @param full_title
#' @param short_title
#' @param publication_facts
#' @param repo_citations
#' @param subm_name
#' @param product_id
#' @param product_name
#' @param product_version
#' @param business_name
#' @param business_address
#' @param data_name
#' @param data_pubdate
#' @param data_pubtime
#' @param data_copyright
#' @param gedcom_version
#' @param ext_tags
#' @param source
#' @param destination
#' @param creation_date
#' @param creation_time
#' @param gedcom_copyright
#' @param default_language
#' @param default_place_form
#' 
#'  
#' 
#' 
#' @param xref The cross-reference identifier for this record. You should not edit this at all
#' as maintenance of these is done automatically.
#' @param indi_xref The cross-reference identifier of an individual record. If the individual
#' does not have a record, then this can be left blank and the value "@VOID@" will be used. However,
#' you will need to define an @indi_phrase.
#' @param fam_xref The cross-reference identifier of a family record.
#' @param sour_xref The cross-reference identifier of a source record. If the source
#' does not have a record, then this can be left blank and the value "@VOID@" will be used. However,
#' you will need to describe the source in @where.
#' @param repo_xref The cross-reference identifier of a repository record. If the repository
#' does not have a record, then this can be left blank and the value "@VOID@" will be used. However,
#' you will need to describe the repository in @notes.
#' @param media_xref The cross-reference identifier of a multimedia record.
#' @param note_xrefs A character vector of relevant note record cross-reference identifiers.
#' @param data_note_xrefs
#' @param subm_xref The cross-reference identifier of a submitter record.
#' @param subm_xrefs A character vector of relevant submitter record cross-reference identifiers.
#' @param husb_xref,wife_xref,chil_xrefs The cross-reference identifier(s) of the member's individual records.
#' If the individual does not have a record, then the value "@VOID@" can be used. 
#' However, you will need to describe the individual by using a named vector (a description can be used
#' in either case), e.g. c("Joe Bloggs" = "@VOID@")
#' @param alia_xrefs A named character vector of relevant individual record cross-reference identifiers
#' whose records also represent this individual. The vector names may provide a description of these records.
#' @param anci_xrefs,desi_xrefs A character vector of relevant submitter record cross-reference identifiers
#' who are interested in the ancestors/descendants of this individual.
#' 
#' @param unique_ids A character vector of enduring and globally-unique identifiers. These need
#' to be formatted in line with RFC 4122 and can be generated with `uuid::UUIDgenerate()`.
#' @param user_ids fwe
#' @param ext_ids dsfse
#' 
#' 
#' @param media_links Associated multimedia. This can either be a `class_media_link` object, a list of them,
#' or a character vector of XREFs of multimedia records. If a character vector is provided then only the XREFs themselves
#' can be recorded (and not associated information). This option is easier if 
#' associated information is not required. 
#' @param notes Associated notes. This can either be a `class_note` object, a list of them,
#' or a character vector of notes. If a character vector is provided then only the notes themselves
#' can be recorded (and not associated information). This option is easier if 
#' associated information is not required. 
#' @param data_notes
#' @param citations Associated sources. This can either be a `class_citation` object, a list of them,
#' or a character vector of XREFs of source records. If a character vector is provided then only the XREFs themselves
#' can be recorded (and not associated information). This option is easier if 
#' associated information is not required. 
#' 
#' 
#' @param hour The hour of the day given as an integer between 0 and 23.
#' @param minute The minute of the hour given as an integer between 0 and 59.
#' @param second The second of the minute given as an integer between 0 and 59.
#' @param fraction The fraction of the second given as an integer.
#' @param utc Whether the time is in Coordinated Universal Time (UTC) (TRUE, the default) or
#' is in local time (FALSE).
#' @param year The year given as an integer (greater than 0).
#' @param month The month of the year given as an integer between 1 and 12.
#' @param day The day of the month given as an integer between 1 and 31.
#' @param bce Whether the date is Before the Common Era. This is FALSE by default,
#' but if TRUE, only the year should be given.
#' @param about Whether the date is near to the date given.
#' @param calc Whether the date is calculated from other values.
#' @param est Whether the date is near to the date given, and is calculated from other values.
#' 
#' @param date_greg A Gregorian date given either as a formatted GEDCOM string, or a
#' `class_date_greg` object.
#' @param date_exact An exact date given either as a formatted GEDCOM string, or a
#' `class_date_exact` object. If not given, it will default to today's date.
#' @param date_period A date period given either as a formatted GEDCOM string, or a
#' `class_date_period` object.
#' @param date The date given either as a formatted GEDCOM string, or a
#' `class_date_value` object.
#' @param date_sort The date given either as a formatted GEDCOM string, or a
#' `class_date_sort` object.
#' @param date_phrase Textual information that cannot be expressed in the date.
#' @param start_date The start of the period/range given either as a formatted GEDCOM string, or a
#' `class_date_greg` object.
#' @param end_date The end of the period/range given either as a formatted GEDCOM string, or a
#' `class_date_greg` object.
#' @param time The time given either as a formatted GEDCOM string, or a
#' `class_time` object.
#' @param created A `class_creation_date` object containing the date the record was created.
#' Creating an object with no parameters sets the date to today.
#' @param updated A `class_change_date` object containing the date the record was updated.
#' Creating an object with no parameters sets the date to today.
#' 
#' @param update_change_dates Whether to automatically update change dates when updating records.
#' This happens when the record is pushed to the gedcom object.
#' @param add_creation_dates Whether to automatically add creation dates when creating records.
#' This happens when the record is pushed to the gedcom object.
#' 
NULL


#' Define properties in GEDCOM 7.0 specification
#' 
#' Define all properties used in S7 classes.
#' 
#' @details This empty function serves as a single location where all properties are defined, 
#' mainly for efficiency and maintainability.
#' @name prop_definitions
#' 
#' @param full A full address as it would appear on a mailing label, with lines separated
#' by semi-colon and a space. For example:
#' "The White House; 1600 Pennsylvania Avenue N.W.; Washington, DC 20500; United States of America"
#' @param adr1,adr2,adr3 Deprecated.
#' @param city The city component of the address.
#' @param state The state component of the address.
#' @param postal_code The postal code component of the address.
#' @param country The country component of the address.
#' @param text A character string. New lines are created with \\n.
#' @param language A character string of language tags as defined in BCP 47.
#' @param languages A character vector of language tags as defined in BCP 47.
#' @param media_type The media type as defined in RFC 2045.
#' @param translations A `TranslationText` object or a list of them.
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
#' either be a `TranslationText` object, a list of them, or a character vector of text. 
#' If any `TranslationText` objects are provided, then you will be forced to define an
#' associated language or media type.
#' @param fact_type TODO
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
#' @param relation_is The nature of the association. This must be a value from `val_roles()`.
#' If a value of "OTHER" is used, a @relation_phrase must be given.
#' @param relation_phrase Textual information that cannot be expressed in the relation.
#' @param place_name A comma-separated string of region names, ordered from smallest to 
#' largest. The specific meaning of each element is given by the @place_form, or in the 
#' `@default_place_form` of the gedcom object if there is no @place_form defined. Elements 
#' should be left blank if they are unknown, do not apply to the location, or are too 
#' specific for the region in question. For example "Baltimore, , Maryland, USA".
#' @param place_form A comma-separated string of jurisdictional titles, which has the same 
#' number of elements as @place_form. For example "City, County, State, Country".
#' @param place_translations A named character vector of translations of the place name.
#' The vector values must follow the same form as the @place_name and the vector names
#' must be a language value as defined by @language.
#' @param lat_long The latitude and longitude of the place, separated by a space.
#' The latitude coordinate is the direction North or South from the equator in degrees and 
#' fraction of degrees. The longitude coordinate is in degrees and fraction of degrees East 
#' or West of the zero or base meridian coordinate.
#' For example: 18 degrees, 9 minutes, and 3.4 seconds North, 168 degrees, 9 minutes, and 
#' 3.4 seconds East would be formatted as "N18.150944 E168.150944".
#' @param fact_types A character string indicating the types
#' of events that were recorded in a particular source. Each event type is separated by a
#' comma and space. For example, a parish register of births, deaths, and marriages
#' would be BIRT, DEAT, MARR. 
#' @param territory The territory associated with the events covered. This can either be a 
#' `Place` object or a character string (a comma-separated string of region names, 
#' ordered from smallest to largest). If a character string is provided then only the 
#' region names can be recorded (and not associated information). This option is easier if 
#' associated information is not required. 
#' @param pedigree TODO
#' @param pedigree_phrase TODO
#' @param confidence TODO
#' @param confidence_phrase TODO
#' @param call_numbers Call number(s) used to file and retrieve items from the repository. 
#' This can either be a `SourceCallNumber` object, a list of them,
#' or a character vector of call numbers. If a character vector is provided then only the 
#' call numbers themselves can be recorded (and not associated medium). This option is easier if 
#' associated information is not required. 
#' @param call_number The call number.
#' @param location TODO
#' @param medium TODO
#' @param medium_phrase TODO
#' @param media_alt A named vector of the media in alternative media forms, c(form = location)
#' @param prefix The name prefix, e.g. Cmdr.
#' @param given The given name or earned name.
#' @param nickname A descriptive or familiar name that is used instead of, or in addition to, one’s proper
#' name.
#' @param surname_prefix Surname prefix or article used in a family name. 
#' For example in the name "de la Cruz", this value would be "de la".
#' @param surname Surname or family name.
#' @param suffix Name piece that appears after the given name and surname parts, e.g. Jr.
#' @param name_pieces A `PersonalNamePieces` object defining the pieces of the full name.
#' @param pers_name TODO
#' @param name_type TODO
#' @param type_phrase TODO
#' @param name_translations TODO
#' @param event_type TODO
#' @param fact_val TODO
#' @param fact_desc TODO
#' @param place see territory
#' @param address The address given either as a `Address` object or as a character string.
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
#' @param associations Associated individuals. This can either be a `Association` object or a list of them.
#' @param age,husb_age,wife_age A character string that indicates the age in years, months, weeks and/or days 
#' that the individual was at the time of the fact. Any combination of these is permitted. 
#' Any labels must come after their corresponding number, for example; "4y 8m 1w 3d". 
#' Age bounds can also be included, for example; "< 40y". If the age doesn't fit this format then describe the age 
#' in the corresponding phrase parameter.
#' @param age_phrase,husb_age_phrase,wife_age_phrase Textual information that cannot be expressed in the age.
#' @param adop_parent TODO
#' @param adop_parent_phrase TODO
#' @param pers_names TODO
#' @param sex The sex of the individual. Either "M" (male), "F" (female), "X" (other), or
#' "U" (undetermined, the default),
#' @param fam_links_chil TODO
#' @param fam_links_spou TODO
#' @param files A `MediaFile` object or a list of them. This refers to 1 or more external 
#' digital files. Grouped files should each pertain to the same context.
#' @param repo_name The name of the repository.
#' @param facts_recorded The facts recorded by the source. This can either be a `FactsRecorded` object, 
#' a list of them, or a character vector of comma-delimited fact types. If a character vector is 
#' provided then only the fact types themselves can be recorded (and not associated information). 
#' This option is easier if associated information is not required. For example, a parish register of 
#' births, deaths, and marriages would be "BIRT, DEAT, MARR". The `val_fact_types()` function gives a
#' list of possible fact types.
#' @param subm_name The name of the submitter.
#' @param originator The person, agency, or entity who created the record. For a published work, 
#' this could be the author, compiler, transcriber, abstractor, or editor. For an unpublished 
#' source, this may be an individual, a government agency, church organization, or private organization.
#' @param full_title The full title of the source.
#' @param short_title A shortened name of the source used for sorting, filing, and retrieving records.
#' @param publication_facts When and where the record was created. For published works, this 
#' includes information such as the city of publication, name of the publisher, and year of publication.
#' @param repo_citations Associated repositories. This can either be a `RepositoryCitation` object, 
#' a list of them, or a character vector of XREFs of repository records. If a character vector is 
#' provided then only the XREFs themselves can be recorded (and not associated information). 
#' This option is easier if associated information is not required.
#' @param subm_name The name of the submitter.
#' @param product_id TODO
#' @param product_name TODO
#' @param product_version TODO
#' @param business_name TODO
#' @param business_address TODO
#' @param data_name TODO
#' @param data_pubdate TODO
#' @param data_pubtime TODO
#' @param data_copyright TODO
#' @param gedcom_version TODO
#' @param ext_tags TODO
#' @param source TODO
#' @param destination TODO
#' @param gedcom_copyright TODO
#' @param default_language TODO
#' @param default_place_form TODO
#' 
#' @param facts TODO
#' @param non_events TODO
#' @param spouse_sealings TODO
#' @param ordinances TODO
#' @param ord_type TODO
#' @param temple_name TODO
#' @param ord_state TODO
#' 
#' 
#' @param xref The cross-reference identifier for this record. You should not edit this at all
#' as maintenance of these is done automatically.
#' @param indi_xref The cross-reference identifier of an individual record. If the individual
#' does not have a record, then this can be left blank and the value "@VOID@" will be used. However,
#' you should define an @indi_phrase.
#' @param fam_xref The cross-reference identifier of a family record.
#' @param sour_xref The cross-reference identifier of a source record. If the source
#' does not have a record, then this can be left blank and the value "@VOID@" will be used. However,
#' you should describe the source in @where.
#' @param repo_xref The cross-reference identifier of a repository record. If the repository
#' does not have a record, then this can be left blank and the value "@VOID@" will be used. However,
#' you should describe the repository in @notes.
#' @param media_xref The cross-reference identifier of a multimedia record.
#' @param note_xrefs A character vector of relevant note record cross-reference identifiers.
#' @param data_note_xrefs A character vector of note record cross-reference identifiers relevant
#' to the source data.
#' @param subm_xref The cross-reference identifier of a submitter record.
#' @param subm_xrefs A character vector of relevant submitter record cross-reference identifiers.
#' @param husb_xref,wife_xref,chil_xrefs The cross-reference identifier(s) of the member's individual records.
#' If the individual does not have a record, then the value "@VOID@" can be used. 
#' However, you will need to describe the individual by using a named vector (a description can be used
#' in either case), e.g. c("Joe Bloggs" = "@VOID@") or c("Joe Bloggs" = "@I1@")
#' @param alia_xrefs A named character vector of relevant individual record cross-reference identifiers
#' whose records also represent this individual. The vector names may provide a description of these records.
#' @param anci_xrefs,desi_xrefs A character vector of relevant submitter record cross-reference identifiers
#' who are interested in the ancestors/descendants of this individual.
#' 
#' @param unique_ids A character vector of enduring and globally-unique identifiers. These need
#' to be formatted in line with RFC 4122 and can be generated with `uuid::UUIDgenerate()`.
#' @param user_ids A character vector of user-generated identifiers. The type of the identifiers can
#' be given in the vector names, e.g. c("Driving license number" = "ABC123")
#' @param ext_ids A named character vector of identifiers maintained by an external authority.
#' The names must be given as a URI. See the GEDCOM specification for more information.
#' 
#' 
#' @param media_links Associated multimedia. This can either be a `MediaLink` object, a list of them,
#' or a character vector of XREFs of multimedia records. If a character vector is provided then only the XREFs themselves
#' can be recorded (and not associated information). This option is easier if 
#' associated information is not required. 
#' @param notes Associated notes. This can either be a `Note` object, a list of them,
#' or a character vector of notes. If a character vector is provided then only the notes themselves
#' can be recorded (and not associated information). This option is easier if 
#' associated information is not required. 
#' @param data_notes Associated notes about the source data. This can either be a `Note` 
#' object, a list of them, or a character vector of notes. If a character vector is provided 
#' then only the notes themselves can be recorded (and not associated information). 
#' This option is easier if associated information is not required.
#' @param citations Associated sources. This can either be a `SourceCitation` object, a list of them,
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
#' `DateGregorian` object.
#' @param date_exact An exact date given either as a formatted GEDCOM string, or a
#' `DateExact` object. If not given, it will default to today's date.
#' @param date_period A date period given either as a formatted GEDCOM string, or a
#' `DatePeriod` object.
#' @param date The date given either as a formatted GEDCOM string, or a
#' `DateValue` object.
#' @param date_sort The date given either as a formatted GEDCOM string, or a
#' `DateSorting` object.
#' @param date_phrase Textual information that cannot be expressed in the date.
#' @param start_date The start of the period/range given either as a formatted GEDCOM string, or a
#' `DateGregorian` object.
#' @param end_date The end of the period/range given either as a formatted GEDCOM string, or a
#' `DateGregorian` object.
#' @param time The time given either as a formatted GEDCOM string, or a
#' `Time` object.
#' @param created A `CreationDate` object containing the date the record was created.
#' Creating an object with no parameters sets the date to today.
#' @param updated A `ChangeDate` object containing the date the record was updated.
#' Creating an object with no parameters sets the date to today.
#' @param creation_date The creation date of the file given either as a formatted GEDCOM string, or a
#' `DateExact` object.
#' @param creation_time The creation time of the file given either as a formatted GEDCOM string, or a
#' `Time` object.
#' @param state_date The ordinance date given either as a formatted GEDCOM string, or a
#' `DateExact` object.
#' @param state_time The ordinance time given either as a formatted GEDCOM string, or a
#' `Time` object.
#' 
#' @param update_change_dates Whether to automatically update change dates when updating records.
#' This happens when the record is pushed to the gedcom object.
#' @param add_creation_dates Whether to automatically add creation dates when creating records.
#' This happens when the record is pushed to the gedcom object.
#' @param subm,indi,fam,sour,repo,media,note A named list containing character vector representations
#' of GEDCOM records. Do not edit these parameters directly.
#' @param xref_prefixes A named vector containing any alphanumeric string (up to 6 characters long) 
#' which will precede the number given to identify new records (of which there are 7 types). 
#' This vector must be of a particular length with specific names. Default value:
#' c(subm = "U", indi = "I", fam = "F", sour = "S", repo = "R", media = "M", note = "N")
#' The order that these records appear in the vector will also dictate the order in which records 
#' will appear in the exported file.
#' @keywords internal
NULL

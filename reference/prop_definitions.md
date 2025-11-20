# Define properties in GEDCOM 7.0 specification

Define common properties used in S7 classes.

## Arguments

- text:

  A character string. New lines are created with \n.

- language:

  A character string of language tags as defined in BCP 47.

- media_type:

  The media type as defined in RFC 2045.

- translations:

  A
  [`TranslationText()`](https://jl5000.github.io/gedcomS7/reference/TranslationText.md)
  object or a list of them. One for each alternate translation of the
  text.

- title:

  The title of the multimedia record.

- source_text:

  A verbatim copy of any description contained within the source. This
  can either be a
  [`TranslationText()`](https://jl5000.github.io/gedcomS7/reference/TranslationText.md)
  object, a list of them, or a character vector of text.

- fact_type:

  A code indicating the type of fact. This must be taken from one of
  [`val_individual_event_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md),
  [`val_individual_attribute_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md),
  [`val_family_event_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md),
  or
  [`val_family_attribute_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md).
  A generic event ("EVEN") or attribute ("FACT") can also be defined for
  more bespoke facts. See
  [`fact_rules_df()`](https://jl5000.github.io/gedcomS7/reference/fact_rules_df.md)
  for the set of rules surrounding the codes allowed.

- fact_val:

  A value associated with the fact. For example for "NCHI" this would be
  the number of children. See
  [`fact_rules_df()`](https://jl5000.github.io/gedcomS7/reference/fact_rules_df.md)
  for the set of rules surrounding the need for values and the values
  allowed.

- fact_desc:

  A further classification of the fact. This is required for generic
  events or attributes. See
  [`fact_rules_df()`](https://jl5000.github.io/gedcomS7/reference/fact_rules_df.md)
  for the set of rules surrounding the need for this.

- medium:

  A value from
  [`val_medium_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md).
  If "OTHER" is selected then a `@medium_phrase` must be given.

- medium_phrase:

  A free text description of the medium. This is mandatory if `@medium`
  is "OTHER".

- media_alt:

  A named vector of the media in alternative media forms, c(form =
  location)

- pers_name:

  The full name of the individual. Surnames should be enclosed in
  forward slashes.

- name_pieces:

  A
  [`PersonalNamePieces()`](https://jl5000.github.io/gedcomS7/reference/PersonalNamePieces.md)
  object defining the pieces of the full name.

- place:

  The associated place. This can either be a
  [`Place()`](https://jl5000.github.io/gedcomS7/reference/Place.md)
  object or a character string (a comma-separated string of region
  names, ordered from smallest to largest).

- address:

  The address given either as a
  [`Address()`](https://jl5000.github.io/gedcomS7/reference/Address.md)
  object or as a character string. This would be as written on a mailing
  label with new lines separated by \n.

- phone_numbers:

  A character vector of phone numbers.

- emails:

  A character vector of email addresses.

- faxes:

  A character vector of fax numbers.

- web_pages:

  A character vector of web page URLs.

- agency:

  The organization, institution, corporation, person, or other entity
  that has responsibility for the associated fact. Examples are an
  employer of a person of an associated occupation, or an educational
  establishment that has awarded a scholastic award.

- relig_affil:

  A religious denomination associated with the fact.

- cause:

  Used in special cases to record the reasons which precipitated the
  fact (e.g. cause of death).

- confidential:

  A logical value indicating whether the associated record/fact should
  be treated as confidential. This allows them to be excluded on export.

- locked:

  A logical value indicating whether the associated record/fact should
  be treated as read-only.

- private:

  A logical value indicating whether the associated record/fact should
  be treated as private. This allows them to be excluded on export.

- associations:

  Associated individuals. This can either be a
  [`Association()`](https://jl5000.github.io/gedcomS7/reference/Association.md)
  object or a list of them.

- age:

  A character string that indicates the age in years, months, weeks
  and/or days that the individual was at the time of the fact. Any
  combination of these is permitted. Any labels must come after their
  corresponding number, for example; "4y 8m 1w 3d". Age bounds can also
  be included, for example; "\< 40y". If the age doesn't fit this format
  then describe the age in the corresponding phrase parameter.

- husb_age:

  A character string that indicates the age in years, months, weeks
  and/or days that the husband was at the time of the fact. Any
  combination of these is permitted. Any labels must come after their
  corresponding number, for example; "4y 8m 1w 3d". Age bounds can also
  be included, for example; "\< 40y". If the age doesn't fit this format
  then describe the age in the corresponding phrase parameter.

- wife_age:

  A character string that indicates the age in years, months, weeks
  and/or days that the wife was at the time of the fact. Any combination
  of these is permitted. Any labels must come after their corresponding
  number, for example; "4y 8m 1w 3d". Age bounds can also be included,
  for example; "\< 40y". If the age doesn't fit this format then
  describe the age in the corresponding phrase parameter.

- age_phrase:

  Free text information that cannot be expressed in the individual's
  age.

- husb_age_phrase:

  Free text information that cannot be expressed in the husband's age.

- wife_age_phrase:

  Free text information that cannot be expressed in the wife's age.

- temple_name:

  The name of a temple of The Church of Jesus Christ of Latter-day
  Saints.

- ord_state:

  An optional value from `val_ordinance_states(@ord_type)`.

- state_date:

  The ordinance date given either as a formatted GEDCOM string, or a
  `DateExact` object.

- state_time:

  The ordinance time given either as a formatted GEDCOM string, or a
  `Time` object.

- XREF:

  The cross-reference identifier for this record. You should not edit
  this at all as maintenance of these is done automatically.

- fam_xref:

  The cross-reference identifier of a family record.

- note_xrefs:

  A character vector of relevant note record cross-reference
  identifiers.

- subm_xrefs:

  A character vector of relevant submitter record cross-reference
  identifiers.

- unique_ids:

  A character vector of enduring and globally-unique identifiers. These
  need to be formatted in line with RFC 4122 and can be generated with
  [`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html).

- user_ids:

  A character vector of user-generated identifiers. The type of the
  identifiers can be given in the vector names, e.g. c("Driving license
  number" = "ABC123")

- ext_ids:

  A named character vector of identifiers maintained by an external
  authority. The names must be given as a URI. See the GEDCOM
  specification for more information.

- media_links:

  Associated multimedia. This can either be a
  [`MediaLink()`](https://jl5000.github.io/gedcomS7/reference/MediaLink.md)
  object, a list of them, or a character vector of XREFs of multimedia
  records.

- notes:

  Associated notes. This can either be a
  [`Note()`](https://jl5000.github.io/gedcomS7/reference/Note.md)
  object, a list of them, or a character vector of notes.

- citations:

  Associated sources. This can either be a
  [`SourceCitation()`](https://jl5000.github.io/gedcomS7/reference/SourceCitation.md)
  object, a list of them, or a character vector of XREFs of source
  records.

- year:

  The year given as an integer (greater than 0).

- month:

  The month of the year given as an integer between 1 and 12.

- day:

  The day of the month given as an integer between 1 and 31.

- date_exact:

  An exact date given either as a formatted GEDCOM string, or a
  [`DateExact()`](https://jl5000.github.io/gedcomS7/reference/DateExact.md)
  object. If not given, it will default to today's date.

- date_period:

  A date period given either as a formatted GEDCOM string, or a
  [`DatePeriod()`](https://jl5000.github.io/gedcomS7/reference/DatePeriod.md)
  object.

- date:

  The date given either as a formatted GEDCOM string, or a
  [`DateValue()`](https://jl5000.github.io/gedcomS7/reference/DateValue.md)
  object.

- date_sort:

  The date given either as a formatted GEDCOM string, or a
  [`DateSorting()`](https://jl5000.github.io/gedcomS7/reference/DateSorting.md)
  object.

- date_phrase:

  Textual information that cannot be expressed in the date.

- start_date:

  The start of the period/range given either as a formatted GEDCOM
  string, or a
  [`DateCalendar()`](https://jl5000.github.io/gedcomS7/reference/DateCalendar.md)
  object.

- end_date:

  The end of the period/range given either as a formatted GEDCOM string,
  or a
  [`DateCalendar()`](https://jl5000.github.io/gedcomS7/reference/DateCalendar.md)
  object.

- time:

  The time given either as a formatted GEDCOM string, or a
  [`Time()`](https://jl5000.github.io/gedcomS7/reference/Time.md)
  object.

- created:

  A
  [`CreationDate()`](https://jl5000.github.io/gedcomS7/reference/CreationDate.md)
  object containing the date the record was created. Creating an object
  with no parameters sets the date to today.

- updated:

  A
  [`ChangeDate()`](https://jl5000.github.io/gedcomS7/reference/ChangeDate.md)
  object containing the date the record was updated. Creating an object
  with no parameters sets the date to today.

## Details

This empty function serves as a single location where all shared
properties are defined, mainly for efficiency and maintainability.

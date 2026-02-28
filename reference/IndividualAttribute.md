# Create an individual attribute object

Create an individual attribute object

## Usage

``` r
IndividualAttribute(
  fact_type = character(0),
  fact_val = character(0),
  fact_desc = character(0),
  date = character(0),
  place = NULL,
  address = NULL,
  phone_numbers = character(0),
  emails = character(0),
  faxes = character(0),
  web_pages = character(0),
  agency = character(0),
  relig_affil = character(0),
  cause = character(0),
  confidential = FALSE,
  locked = FALSE,
  private = FALSE,
  date_sort = character(0),
  associations = list(),
  note_xrefs = character(0),
  notes = list(),
  citations = list(),
  media_links = list(),
  unique_ids = character(0),
  age = character(0),
  age_phrase = character(0)
)
```

## Arguments

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

- date:

  The date given either as a formatted GEDCOM string, or a
  [`DateValue()`](https://jl5000.github.io/gedcomS7/reference/DateValue.md)
  object.

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

- date_sort:

  The date given either as a formatted GEDCOM string, or a
  [`DateSorting()`](https://jl5000.github.io/gedcomS7/reference/DateSorting.md)
  object.

- associations:

  Associated individuals. This can either be a
  [`Association()`](https://jl5000.github.io/gedcomS7/reference/Association.md)
  object or a list of them.

- note_xrefs:

  A character vector of relevant note record cross-reference
  identifiers.

- notes:

  Associated notes. This can either be a
  [`Note()`](https://jl5000.github.io/gedcomS7/reference/Note.md)
  object, a list of them, or a character vector of notes.

- citations:

  Associated sources. This can either be a
  [`SourceCitation()`](https://jl5000.github.io/gedcomS7/reference/SourceCitation.md)
  object, a list of them, or a character vector of XREFs of source
  records.

- media_links:

  Associated multimedia. This can either be a
  [`MediaLink()`](https://jl5000.github.io/gedcomS7/reference/MediaLink.md)
  object, a list of them, or a character vector of XREFs of multimedia
  records.

- unique_ids:

  A character vector of enduring and globally-unique identifiers. These
  need to be formatted in line with RFC 9562 and can be generated with
  [`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html).

- age:

  A character string that indicates the age in years, months, weeks
  and/or days that the individual was at the time of the fact. Any
  combination of these is permitted. It is recommended that this be an
  age from a cited source document. Any labels must come after their
  corresponding number, for example; "4y 8m 1w 3d". Age bounds can also
  be included, for example; "\< 40y". If the age doesn't fit this format
  then describe the age in the corresponding phrase parameter.

- age_phrase:

  Free text information that cannot be expressed in the individual's
  age.

## Value

An S7 object representing a GEDCOM INDIVIDUAL_ATTRIBUTE_STRUCTURE.

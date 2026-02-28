# Create an individual record object

Create an individual record object

## Usage

``` r
IndividualRecord(
  XREF = "@GEDCOMS7_ORPHAN@",
  confidential = FALSE,
  locked = FALSE,
  private = FALSE,
  user_ids = character(0),
  unique_ids = character(0),
  ext_ids = character(0),
  note_xrefs = character(0),
  notes = list(),
  citations = list(),
  media_links = list(),
  created = NULL,
  updated = NULL,
  pers_names = list(),
  sex = "U",
  facts = list(),
  non_events = list(),
  ordinances = list(),
  fam_links_chil = list(),
  fam_links_spou = list(),
  subm_xrefs = character(0),
  associations = list(),
  alia_xrefs = character(0),
  anci_xrefs = character(0),
  desi_xrefs = character(0)
)
```

## Arguments

- XREF:

  The cross-reference identifier for this record. You should not edit
  this at all as maintenance of these is done automatically.

- confidential:

  A logical value indicating whether the associated record/fact should
  be treated as confidential. This allows them to be excluded on export.

- locked:

  A logical value indicating whether the associated record/fact should
  be treated as read-only.

- private:

  A logical value indicating whether the associated record/fact should
  be treated as private. This allows them to be excluded on export.

- user_ids:

  A character vector of user-generated identifiers. The type of the
  identifiers can be given in the vector names, e.g. c("Driving license
  number" = "ABC123")

- unique_ids:

  A character vector of enduring and globally-unique identifiers. These
  need to be formatted in line with RFC 9562 and can be generated with
  [`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html).

- ext_ids:

  A named character vector of identifiers maintained by an external
  authority. The names must be given as a URI. See the GEDCOM
  specification for more information.

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

- pers_names:

  A
  [`PersonalName()`](https://jl5000.github.io/gedcomS7/reference/PersonalName.md)
  object or a list of them, giving the names of this individual. A
  simple character vector of names can be provided instead, but this is
  not recommended.

- sex:

  The sex of the individual. Either "M" (male), "F" (female), "X"
  (other), or "U" (undetermined, the default).

- facts:

  Events and/or attributes for this individual. An
  [`IndividualEvent()`](https://jl5000.github.io/gedcomS7/reference/IndividualEvent.md)/[`IndividualAttribute()`](https://jl5000.github.io/gedcomS7/reference/IndividualAttribute.md)
  object, or a list of them.

- non_events:

  Events that this individual did not experience. A
  [`NonEvent()`](https://jl5000.github.io/gedcomS7/reference/NonEvent.md)
  object, or a list of them.

- ordinances:

  An
  [`Ordinance()`](https://jl5000.github.io/gedcomS7/reference/Ordinance.md)
  object, or a list of them.

- fam_links_chil:

  A
  [`FamilyLinkChild()`](https://jl5000.github.io/gedcomS7/reference/FamilyLinkChild.md)
  object or a list of them, giving the Family records that this
  individual is a member of as a child. A character vector of Family
  record xrefs can also be provided. This will be automatically updated
  if the individual's membership in a Family record changes.

- fam_links_spou:

  A
  [`FamilyLinkSpouse()`](https://jl5000.github.io/gedcomS7/reference/FamilyLinkSpouse.md)
  object or a list of them, giving the Family records that this
  individual is a member of as a spouse. A character vector of Family
  record xrefs can also be provided. This will be automatically updated
  if the individual's membership in a Family record changes.

- subm_xrefs:

  A character vector of relevant submitter record cross-reference
  identifiers.

- associations:

  Associated individuals. This can either be a
  [`Association()`](https://jl5000.github.io/gedcomS7/reference/Association.md)
  object or a list of them.

- alia_xrefs:

  A named character vector of relevant individual record cross-reference
  identifiers whose records also represent this individual. The vector
  names may provide a description of these records.

- anci_xrefs:

  A character vector of relevant submitter record cross-reference
  identifiers who are interested in the ancestors of this individual.

- desi_xrefs:

  A character vector of relevant submitter record cross-reference
  identifiers who are interested in the descendants of this individual.

## Value

An S7 object representing a GEDCOM INDIVIDUAL_RECORD.

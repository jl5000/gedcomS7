# Create a family record object

Create a family record object

## Usage

``` r
FamilyRecord(
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
  facts = list(),
  non_events = list(),
  husb_xref = character(0),
  wife_xref = character(0),
  chil_xrefs = character(0),
  associations = list(),
  subm_xrefs = character(0),
  spouse_sealings = list()
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
  need to be formatted in line with RFC 4122 and can be generated with
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

- facts:

  Events and/or attributes for this family. A
  [`FamilyEvent()`](https://jl5000.github.io/gedcomS7/reference/FamilyEvent.md)/[`FamilyAttribute()`](https://jl5000.github.io/gedcomS7/reference/FamilyAttribute.md)
  object, or a list of them.

- non_events:

  Events that this family did not experience. A
  [`NonEvent()`](https://jl5000.github.io/gedcomS7/reference/NonEvent.md)
  object, or a list of them.

- husb_xref, wife_xref, chil_xrefs:

  The cross-reference identifier(s) of member's individual records. If
  the individual does not have a record, then the value "@VOID@" can be
  used. However, you will need to describe the individual by using a
  named vector (a description can be used in either case), e.g. c("Joe
  Bloggs" = "@VOID@") or c("Joe Bloggs" = "@I1@").

- associations:

  Associated individuals. This can either be a
  [`Association()`](https://jl5000.github.io/gedcomS7/reference/Association.md)
  object or a list of them.

- subm_xrefs:

  A character vector of relevant submitter record cross-reference
  identifiers.

- spouse_sealings:

  A
  [`SpouseSealing()`](https://jl5000.github.io/gedcomS7/reference/SpouseSealing.md)
  object or a list of them detailing the sealing of a husband and wife
  in a temple ceremony of The Church of Jesus Christ of Latter-day
  Saints.

## Value

An S7 object representing a GEDCOM FAMILY_RECORD.

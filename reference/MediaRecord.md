# Create a multimedia record object

Create a multimedia record object

## Usage

``` r
MediaRecord(
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
  files = list()
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

  Not used.

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

- files:

  A `MediaFile` object or a list of them. This refers to 1 or more
  external digital files. Grouped files should each pertain to the same
  context.

## Value

An S7 object representing a GEDCOM MULTIMEDIA_RECORD.

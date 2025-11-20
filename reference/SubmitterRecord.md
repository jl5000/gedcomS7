# Create a submitter record object

Create a submitter record object

## Usage

``` r
SubmitterRecord(
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
  subm_name = character(0),
  address = NULL,
  phone_numbers = character(0),
  emails = character(0),
  faxes = character(0),
  web_pages = character(0),
  languages = character(0)
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

  Not used.

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

- subm_name:

  The name of the submitter.

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

- languages:

  A character vector of language tags as defined in BCP 47.

## Value

An S7 object representing a GEDCOM SUBMITTER_RECORD.

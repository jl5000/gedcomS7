# Create a source record object

Create a source record object

## Usage

``` r
SourceRecord(
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
  facts_recorded = list(),
  agency = character(0),
  data_note_xrefs = character(0),
  data_notes = list(),
  originator = character(0),
  full_title = character(0),
  short_title = character(0),
  publication_facts = character(0),
  source_text = list(),
  repo_citations = list()
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

- facts_recorded:

  The facts recorded by the source. This can either be a `FactsRecorded`
  object, a list of them, or a character vector of comma-delimited fact
  types. For example, a parish register of births, deaths, and marriages
  would be "BIRT, DEAT, MARR". The
  [`val_fact_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md)
  function gives a list of possible fact types.

- agency:

  The organization, institution, corporation, person, or other entity
  that has responsibility for the associated fact. Examples are an
  employer of a person of an associated occupation, or an educational
  establishment that has awarded a scholastic award.

- data_note_xrefs:

  A character vector of note record cross-reference identifiers relevant
  to the source data.

- data_notes:

  Associated notes about the source data. This can either be a `Note`
  object, a list of them, or a character vector of notes.

- originator:

  The person, agency, or entity who created the record. For a published
  work, this could be the author, compiler, transcriber, abstractor, or
  editor. For an unpublished source, this may be an individual, a
  government agency, church organization, or private organization.

- full_title:

  The full title of the source.

- short_title:

  A shortened name of the source used for sorting, filing, and
  retrieving records.

- publication_facts:

  When and where the record was created. For published works, this
  includes information such as the city of publication, name of the
  publisher, and year of publication.

- source_text:

  A verbatim copy of any description contained within the source. This
  can either be a
  [`TranslationText()`](https://jl5000.github.io/gedcomS7/reference/TranslationText.md)
  object, a list of them, or a character vector of text.

- repo_citations:

  Associated repositories. This can either be a `RepositoryCitation`
  object, a list of them, or a character vector of XREFs of repository
  records.

## Value

An S7 object representing a GEDCOM SOURCE_RECORD.

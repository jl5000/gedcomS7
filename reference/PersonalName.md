# Create a personal name object

Create a personal name object

## Usage

``` r
PersonalName(
  pers_name = character(0),
  name_type = character(0),
  type_phrase = character(0),
  name_pieces = NULL,
  name_translations = list(),
  notes = list(),
  note_xrefs = character(0),
  citations = list()
)
```

## Arguments

- pers_name:

  The full name of the individual. Surnames should be enclosed in
  forward slashes.

- name_type:

  An optional name type taken from
  [`val_name_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md).

- type_phrase:

  An optional free text description of the name type. This is required
  if the name type is "OTHER".

- name_pieces:

  A
  [`PersonalNamePieces()`](https://jl5000.github.io/gedcomS7/reference/PersonalNamePieces.md)
  object defining the pieces of the full name.

- name_translations:

  A
  [`PersonalNameTran()`](https://jl5000.github.io/gedcomS7/reference/PersonalNameTran.md)
  object or a list of them, providing different translations of this
  personal name.

- notes:

  Associated notes. This can either be a
  [`Note()`](https://jl5000.github.io/gedcomS7/reference/Note.md)
  object, a list of them, or a character vector of notes.

- note_xrefs:

  A character vector of relevant note record cross-reference
  identifiers.

- citations:

  Associated sources. This can either be a
  [`SourceCitation()`](https://jl5000.github.io/gedcomS7/reference/SourceCitation.md)
  object, a list of them, or a character vector of XREFs of source
  records.

## Value

An S7 object representing a GEDCOM PERSONAL_NAME_STRUCTURE.

# Create a note structure object

Create a note structure object

## Usage

``` r
Note(
  text = character(0),
  language = character(0),
  media_type = character(0),
  translations = list(),
  citations = list()
)
```

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

- citations:

  Associated sources. This can either be a
  [`SourceCitation()`](https://jl5000.github.io/gedcomS7/reference/SourceCitation.md)
  object, a list of them, or a character vector of XREFs of source
  records.

## Value

An S7 object representing a GEDCOM NOTE_STRUCTURE.

## Details

The shared note (SNOTE) alternative of this structure is defined
separately in relevant structures.

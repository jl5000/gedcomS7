# Create an association object

Create an association object

## Usage

``` r
Association(
  indi_xref = "@VOID@",
  indi_phrase = character(0),
  relation_is = character(0),
  relation_phrase = character(0),
  note_xrefs = character(0),
  notes = list(),
  citations = list()
)
```

## Arguments

- indi_xref:

  The cross-reference identifier of an individual record. If the
  individual does not have a record, then this can be left blank and a
  void xref will be used. However, you should define an @indi_phrase.

- indi_phrase:

  Textual information that cannot be expressed in the @indi_xref.

- relation_is:

  The nature of the association. This must be a value from
  [`val_roles()`](https://jl5000.github.io/gedcomS7/reference/lookups.md).
  If a value of "OTHER" is used, a @relation_phrase must be given.

- relation_phrase:

  Textual information that cannot be expressed in the relation.

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

## Value

An S7 object representing a GEDCOM ASSOCIATION_STRUCTURE.

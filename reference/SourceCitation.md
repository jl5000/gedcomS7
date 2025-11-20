# Create a source citation object

Create a source citation object

## Usage

``` r
SourceCitation(
  sour_xref = "@VOID@",
  where = character(0),
  date = character(0),
  source_text = list(),
  fact_type = character(0),
  fact_phrase = character(0),
  role = character(0),
  role_phrase = character(0),
  certainty = character(0),
  media_links = list(),
  note_xrefs = character(0),
  notes = list()
)
```

## Arguments

- sour_xref:

  The cross-reference identifier of a source record. If the source does
  not have a record, then this can be left blank and a void xref will be
  used. However, you should describe the source in @where.

- where:

  A specific location within the information referenced. For a published
  work, this could include the volume of a multi-volume work and the
  page number or numbers. For a periodical, it could include volume,
  issue, and page numbers. For a newspaper, it could include a date,
  page number, and column number. For an unpublished source or micro‐
  filmed works, this could be a film or sheet number, page number, or
  frame number. A census record might have an enumerating district, page
  number, line number, dwelling number, and family number. It is
  recommended that the data in this field be formatted comma-separated
  with label: value pairs

- date:

  The date given either as a formatted GEDCOM string, or a
  [`DateValue()`](https://jl5000.github.io/gedcomS7/reference/DateValue.md)
  object.

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

- fact_phrase:

  Textual information that cannot be expressed in the fact type.

- role:

  What role this person played in this fact.

- role_phrase:

  Textual information that cannot be expressed in the role.

- certainty:

  An enumerated value indicating the credibility of a piece of
  information, based on its supporting evidence. Some systems use this
  feature to rank multiple conflicting opinions for display of most
  likely information first. It is not intended to eliminate the
  receivers’ need to evaluate the evidence for themselves. "0" =
  unreliable/estimated data "1" = Questionable reliability of evidence
  "2" = Secondary evidence, data officially recorded sometime after
  event "3" = Direct and primary evidence used, or by dominance of the
  evidence

- media_links:

  Associated multimedia. This can either be a
  [`MediaLink()`](https://jl5000.github.io/gedcomS7/reference/MediaLink.md)
  object, a list of them, or a character vector of XREFs of multimedia
  records.

- note_xrefs:

  A character vector of relevant note record cross-reference
  identifiers.

- notes:

  Associated notes. This can either be a
  [`Note()`](https://jl5000.github.io/gedcomS7/reference/Note.md)
  object, a list of them, or a character vector of notes.

## Value

An S7 object representing a GEDCOM SOURCE_CITATION.

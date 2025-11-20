# Create a non-event object

Create a non-event object

## Usage

``` r
NonEvent(
  event_type = character(0),
  date_period = character(0),
  date_phrase = character(0),
  note_xrefs = character(0),
  notes = list(),
  citations = list()
)
```

## Arguments

- event_type:

  A code indicating the type of event that didn't happen. This must be
  taken from
  [`val_event_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md).

- date_period:

  A date period given either as a formatted GEDCOM string, or a
  [`DatePeriod()`](https://jl5000.github.io/gedcomS7/reference/DatePeriod.md)
  object.

- date_phrase:

  Textual information that cannot be expressed in the date.

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

An S7 object representing a GEDCOM NON_EVENT_STRUCTURE.

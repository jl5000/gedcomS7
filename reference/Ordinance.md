# Create an individual ordinance object

Create an individual ordinance object

## Usage

``` r
Ordinance(
  ord_type = character(0),
  date = character(0),
  temple_name = character(0),
  place = NULL,
  ord_state = character(0),
  state_date = character(0),
  state_time = character(0),
  note_xrefs = character(0),
  notes = list(),
  citations = list(),
  fam_xref = character(0)
)
```

## Arguments

- ord_type:

  A value from
  [`val_individual_ordinance_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md).

- date:

  The date given either as a formatted GEDCOM string, or a
  [`DateValue()`](https://jl5000.github.io/gedcomS7/reference/DateValue.md)
  object.

- temple_name:

  The name of a temple of The Church of Jesus Christ of Latter-day
  Saints.

- place:

  The associated place. This can either be a
  [`Place()`](https://jl5000.github.io/gedcomS7/reference/Place.md)
  object or a character string (a comma-separated string of region
  names, ordered from smallest to largest).

- ord_state:

  An optional value from `val_ordinance_states(@ord_type)`.

- state_date:

  The ordinance date given either as a formatted GEDCOM string, or a
  `DateExact` object.

- state_time:

  The ordinance time given either as a formatted GEDCOM string, or a
  `Time` object.

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

- fam_xref:

  The cross-reference identifier of a family record.

## Value

An S7 object representing a GEDCOM LDS_INDIVIDUAL_ORDINANCE.

# Create an object recording facts covered in a source record

Create an object recording facts covered in a source record

## Usage

``` r
FactsRecorded(
  fact_types = character(0),
  date_period = character(0),
  date_phrase = character(0),
  territory = NULL
)
```

## Arguments

- fact_types:

  A character string indicating the types of events that were recorded
  in a particular source. Each event type is separated by a comma and
  space. For example, a parish register of births, deaths, and marriages
  would be BIRT, DEAT, MARR.

- date_period:

  A date period given either as a formatted GEDCOM string, or a
  [`DatePeriod()`](https://jl5000.github.io/gedcomS7/reference/DatePeriod.md)
  object.

- date_phrase:

  Textual information that cannot be expressed in the date.

- territory:

  The territory associated with the events covered. This can either be a
  [`Place()`](https://jl5000.github.io/gedcomS7/reference/Place.md)
  object or a character string (a comma-separated string of region
  names, ordered from smallest to largest).

## Value

An S7 object representing a GEDCOM SOUR.EVEN structure.

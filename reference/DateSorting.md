# Create a GEDCOM Sorting Date object

Create a GEDCOM Sorting Date object

## Usage

``` r
DateSorting(
  date = character(0),
  date_phrase = character(0),
  time = character(0)
)
```

## Arguments

- date:

  The date given either as a formatted GEDCOM string, or a
  `DateCalendar` object.

- date_phrase:

  Textual information that cannot be expressed in the date.

- time:

  The time given either as a formatted GEDCOM string, or a
  [`Time()`](https://jl5000.github.io/gedcomS7/reference/Time.md)
  object.

## Value

An S7 object representing a GEDCOM Sorting Date.

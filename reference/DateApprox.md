# Create a GEDCOM Approximate Date object

Create a GEDCOM Approximate Date object

## Usage

``` r
DateApprox(date_cal = character(0), about = TRUE, calc = FALSE, est = FALSE)
```

## Arguments

- date_cal:

  A Calendar date given either as a formatted GEDCOM string, or a
  `DateCalendar` object.

- about:

  Whether the date is near to the date given.

- calc:

  Whether the date is calculated from other values.

- est:

  Whether the date is near to the date given, and is calculated from
  other values.

## Value

An S7 object representing a GEDCOM Approximate Date.

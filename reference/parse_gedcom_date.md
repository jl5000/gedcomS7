# Convert a GEDCOM date into a date object

Convert a GEDCOM date into a date object

## Usage

``` r
parse_gedcom_date(date_string, minimise = TRUE)
```

## Arguments

- date_string:

  A GEDCOM date value string.

- minimise:

  Whether to fill in missing date pieces so that the date is minimised.
  For example, if no month is given, January is used. If minimise =
  FALSE, December will be used.

## Value

A date.

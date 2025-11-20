# Determine the number of years between two dates

Determine the number of years between two dates

## Usage

``` r
date_diff(date1, date2 = NULL, minimise = TRUE)
```

## Arguments

- date1:

  A GEDCOM date string.

- date2:

  A GEDCOM date string. If no date is given, today's date is used.

- minimise:

  If date ranges or periods are used in the dates, whether to choose the
  bounds which assume the minimum date difference. If this is FALSE, the
  maximum date difference is assumed.

## Value

A numeric value giving the number of years. A numeric value less than
zero means no determination could be made.

# Construct a regular expression for DATE values

Construct a regular expression for DATE values

## Usage

``` r
reg_date(flatten = TRUE, only = TRUE, strict = TRUE)
```

## Arguments

- flatten:

  A logical value which determines whether a single regex string should
  be returned (flatten = TRUE) or if a vector of them should be returned
  (flatten = FALSE). The vector output is used if the regexes need to be
  combined with other regexes. If they do not, then they are anchored
  with ^ and \$ and separated with \| (OR).

- only:

  Whether to allow strings of only date. If FALSE, the regular
  expression accepts patterns where text can come before or after the
  date().

- strict:

  Whether to check whether the days are actually valid or just check
  that days are any one or two digit number.

## Value

Either a single regex string or a vector of them.

## Details

The DATE (and subsequent DATE_CALENDAR) pattern can potentially handle
several different calendar types, but this package has only implemented
the Gregorian/Julian calendar.

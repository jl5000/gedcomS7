# Construct a regular expression for DATE_EXACT values

Construct a regular expression for DATE_EXACT values

## Usage

``` r
reg_date_exact(only = TRUE, strict = TRUE)
```

## Arguments

- only:

  Whether to allow strings of only date_exact. If FALSE, the regular
  expression accepts patterns where text can come before or after the
  date_exact().

- strict:

  Whether to check whether the days are actually valid or just check
  that days are any one or two digit number.

## Value

A regex string

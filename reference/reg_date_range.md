# Construct the regex pattern for DATE_RANGE values

Construct the regex pattern for DATE_RANGE values

## Usage

``` r
reg_date_range(flatten = TRUE, strict = TRUE)
```

## Arguments

- flatten:

  A logical value which determines whether a single regex string should
  be returned (flatten = TRUE) or if a vector of them should be returned
  (flatten = FALSE). The vector output is used if the regexes need to be
  combined with other regexes. If they do not, then they are anchored
  with ^ and \$ and separated with \| (OR).

- strict:

  Whether to check whether the days are actually valid or just check
  that days are any one or two digit number.

## Value

Either a single regex string or a vector of them.

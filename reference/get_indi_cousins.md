# Identify all cousins for an individual

Identify all cousins for an individual

## Usage

``` r
get_indi_cousins(x, xref, degree = 1, inc_half = FALSE)
```

## Arguments

- x:

  A gedcom object.

- xref:

  The xref of an Individual record.

- degree:

  Whether to return first cousins (degree = 1), second cousins (degree =
  2), etc.

- inc_half:

  Whether to include half cousins.

## Value

A character vector of xrefs.

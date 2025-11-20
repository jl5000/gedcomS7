# Identify all records that contain a pattern

Identify all records that contain a pattern

## Usage

``` r
get_records_by_pattern(x, pattern, return_context = FALSE)
```

## Arguments

- x:

  A gedcom object.

- pattern:

  A regular expression. Case is ignored.

- return_context:

  Whether to return a named list of matching lines in the GEDCOM (TRUE)
  or just the xrefs (FALSE, default).

## Value

Either a vector of matching xrefs, or a named list of matching GEDCOM
lines.

# Identify unreferenced records

This function identifies records that are not referenced in any other
records.

## Usage

``` r
get_unused_recs(x)
```

## Arguments

- x:

  A gedcom object.

## Value

A character vector of xrefs.

## Details

You would expect every record to be referenced by another in some way.
For example, Individual records should reference Family records (and
vice-versa), Repository records should be referenced by Source records,
and Source records should be cited by other records.

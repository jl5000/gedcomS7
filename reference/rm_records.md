# Remove records from a GEDCOM object

Remove records from a GEDCOM object

## Usage

``` r
rm_records(x, xrefs, void_refs = TRUE)
```

## Arguments

- x:

  A gedcom object.

- xrefs:

  A character vector of xrefs to remove.

- void_refs:

  Whether to replace references to this record with a @VOID@ reference.
  This indicates to people that there was a reference to a record here.
  Note that if this is set to FALSE, you risk losing supplementary
  information (e.g. pedigree data in family links).

## Value

The gedcom object with the records removed.

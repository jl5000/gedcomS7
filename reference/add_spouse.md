# Add a spouse record for an individual

This creates a record for a spouse and potentially their Family record.

## Usage

``` r
add_spouse(x, xref, sex = "U", spou_name = NULL, fam_xref = NULL)
```

## Arguments

- x:

  A gedcom object.

- xref:

  The xref of an Individual record.

- sex:

  The sex of the spouse.

- spou_name:

  Optional name to give to the spouse. Surnames must be enclosed in
  forward slashes.

- fam_xref:

  The cross-reference identifier of the Family record if it already
  exists. If this is not provided, a new Family record will be created.

## Value

A gedcom object with additional spouse and Family records.

# Add parent records for an individual

This function adds records for an individual's parents.

## Usage

``` r
add_parents(x, xref, inc_sex = TRUE, fath_name = NULL, moth_name = NULL)
```

## Arguments

- x:

  A gedcom object.

- xref:

  The xref of an Individual record.

- inc_sex:

  Whether to populate the sex of the parents. This will ensure that
  there is one male and one female parent. Otherwise the sex will be
  assigned as "U" (undetermined).

- fath_name, moth_name:

  Optional names to give to the parents. Surnames must be enclosed in
  forward slashes.

## Value

A gedcom object with additional parent records.

## Details

This function may also create a Family record and will not modify
existing parents.

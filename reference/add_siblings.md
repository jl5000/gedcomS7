# Create sibling records for an individual

Create sibling records for an individual

## Usage

``` r
add_siblings(x, xref, sexes, sib_names = NULL)
```

## Arguments

- x:

  A gedcom object.

- xref:

  The xref of an Individual record.

- sexes:

  A character string giving the sexes of each sibling. For example,
  "FFM" to add two sisters and one brother.

- sib_names:

  A character vector of sibling's names. If provided, it must be the
  same length as the number of sexes. If you don't want to provide a
  name for a sibling, set the name to "".

  Surnames must be enclosed in forward slashes. If all names you supply
  do not contain forward slashes then surnames will be taken from the
  father (or mother).

## Value

A gedcom object with additional sibling records.

## Details

This function may also create a Family record and will not modify
existing siblings.

# Create sibling records for an individual

Create sibling records for an individual

## Usage

``` r
add_siblings(x, xref, sexes, sib_names = NULL, pedigrees = "BIRTH")
```

## Arguments

- x:

  A gedcom object.

- xref:

  The xref of an Individual record.

- sexes:

  A character string giving the sexes (from
  [`val_sexes()`](https://jl5000.github.io/gedcomS7/reference/lookups.md))
  of each sibling. For example, "FFM" to add two sisters and one
  brother.

- sib_names:

  A character vector of sibling's names. If provided, it must be the
  same length as the number of sexes. If you don't want to provide a
  name for a sibling, set the name to "".

  Surnames must be enclosed in forward slashes. If all names you supply
  do not contain forward slashes then surnames will be taken from the
  father (or mother).

- pedigrees:

  A character vector of pedigrees from
  [`val_pedigree_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md).
  This must be a vector of length one (which will be recycled) or the
  same size as the number of siblings. A value of NULL means no
  pedigrees will be defined. If you don't want to provide a pedigree for
  a sibling, set the pedigree to "". Defaults to "BIRTH".

## Value

A gedcom object with additional sibling records.

## Details

This function may also create a Family record and will not modify
existing siblings.

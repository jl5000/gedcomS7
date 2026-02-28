# Create children records for a family

Create children records for a family

## Usage

``` r
add_children(x, xref, sexes, chil_names = NULL, pedigrees = "BIRTH")
```

## Arguments

- x:

  A gedcom object.

- xref:

  The xref of a Family record.

- sexes:

  A character string giving the sexes (from
  [`val_sexes()`](https://jl5000.github.io/gedcomS7/reference/lookups.md))
  of each child. For example, "FFM" to add two daughters and one son.

- chil_names:

  A character vector of children's names. If provided, it must be the
  same length as the number of sexes. If you don't want to provide a
  name for a child, set the name to "".

  Surnames must be enclosed in forward slashes. If all names you supply
  do not contain forward slashes then surnames will be taken from the
  father (or mother).

- pedigrees:

  A character vector of pedigrees from
  [`val_pedigree_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md).
  This must be a vector of length one (which will be recycled) or the
  same size as the number of children. A value of NULL means no
  pedigrees will be defined. If you don't want to provide a pedigree for
  a child, set the pedigree to "". Defaults to "BIRTH".

## Value

A gedcom object with additional child records.

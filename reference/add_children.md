# Create children records for a family

Create children records for a family

## Usage

``` r
add_children(x, xref, sexes, chil_names = NULL)
```

## Arguments

- x:

  A gedcom object.

- xref:

  The xref of a Family record.

- sexes:

  A character string giving the sexes of each child. For example, "FFM"
  to add two daughters and one son.

- chil_names:

  A character vector of children's names. If provided, it must be the
  same length as the number of sexes. If you don't want to provide a
  name for a child, set the name to "".

  Surnames must be enclosed in forward slashes. If all names you supply
  do not contain forward slashes then surnames will be taken from the
  father (or mother).

## Value

A gedcom object with additional child records.

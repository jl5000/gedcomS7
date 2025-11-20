# Create a name translation object

Create a name translation object

## Usage

``` r
PersonalNameTran(
  pers_name = character(0),
  language = character(0),
  name_pieces = NULL
)
```

## Arguments

- pers_name:

  The full name of the individual. Surnames should be enclosed in
  forward slashes.

- language:

  A character string of language tags as defined in BCP 47.

- name_pieces:

  A
  [`PersonalNamePieces()`](https://jl5000.github.io/gedcomS7/reference/PersonalNamePieces.md)
  object defining the pieces of the full name.

## Value

An S7 object representing a GEDCOM personal name translation
substructure.

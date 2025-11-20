# Create a family link (as spouse) object

Create a family link (as spouse) object

## Usage

``` r
FamilyLinkSpouse(
  fam_xref = character(0),
  note_xrefs = character(0),
  notes = list()
)
```

## Arguments

- fam_xref:

  The cross-reference identifier of a family record.

- note_xrefs:

  A character vector of relevant note record cross-reference
  identifiers.

- notes:

  Associated notes. This can either be a
  [`Note()`](https://jl5000.github.io/gedcomS7/reference/Note.md)
  object, a list of them, or a character vector of notes.

## Value

An S7 object representing a GEDCOM family link as a spouse.

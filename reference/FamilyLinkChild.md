# Create a family link (as child) object

Create a family link (as child) object

## Usage

``` r
FamilyLinkChild(
  fam_xref = character(0),
  note_xrefs = character(0),
  notes = list(),
  pedigree = character(0),
  pedigree_phrase = character(0),
  confidence = character(0),
  confidence_phrase = character(0)
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

- pedigree:

  An optional value from
  [`val_pedigree_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md)
  indicating the nature of the link.

- pedigree_phrase:

  An optional free-text phrase describing the nature of the link. This
  is required if `@pedigree` is "OTHER".

- confidence:

  An optional value from
  [`val_confidence_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md)
  indicating the confidence of the link.

- confidence_phrase:

  An optional free-text phrase expanding on the confidence of the link.

## Value

An S7 object representing a GEDCOM family link as a child.

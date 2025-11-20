# Create a source call number object

Create a source call number object

## Usage

``` r
SourceCallNumber(
  call_number = character(0),
  medium = character(0),
  medium_phrase = character(0)
)
```

## Arguments

- call_number:

  The call number.

- medium:

  A value from
  [`val_medium_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md).
  If "OTHER" is selected then a `@medium_phrase` must be given.

- medium_phrase:

  A free text description of the medium. This is mandatory if `@medium`
  is "OTHER".

## Value

An S7 object representing the CALN substructure of a GEDCOM
SOURCE_REPOSITORY_CITATION.

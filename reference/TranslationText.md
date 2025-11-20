# Create a text translation object

Create a text translation object

## Usage

``` r
TranslationText(
  text = character(0),
  language = character(0),
  media_type = character(0)
)
```

## Arguments

- text:

  A character string. New lines are created with \n.

- language:

  A character string of language tags as defined in BCP 47.

- media_type:

  The media type as defined in RFC 2045.

## Value

An S7 object representing a GEDCOM text translation substructure.

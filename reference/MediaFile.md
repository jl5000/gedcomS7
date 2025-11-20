# Create a media file object

Create a media file object

## Usage

``` r
MediaFile(
  location = character(0),
  title = character(0),
  media_type = character(0),
  medium = character(0),
  medium_phrase = character(0),
  media_alt = character(0)
)
```

## Arguments

- location:

  An absolute or relative URL to the file.

- title:

  The title of the multimedia record.

- media_type:

  The media type as defined in RFC 2045.

- medium:

  A value from
  [`val_medium_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md).
  If "OTHER" is selected then a `@medium_phrase` must be given. This
  should describe the original medium from which it was derived. So if
  it is a digital image scanned from a physical photograph, it should be
  "PHOTO" instead of "ELECTRONIC".

- medium_phrase:

  A free text description of the medium. This is mandatory if `@medium`
  is "OTHER".

- media_alt:

  A named vector of the media in alternative media forms, c(form =
  location)

## Value

An S7 object representing a GEDCOM multimedia file substructure.

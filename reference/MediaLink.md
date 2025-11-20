# Create a multimedia link object

Create a multimedia link object

## Usage

``` r
MediaLink(
  media_xref = "@VOID@",
  title = character(0),
  top = integer(0),
  left = integer(0),
  height = integer(0),
  width = integer(0)
)
```

## Arguments

- media_xref:

  The cross-reference identifier of a multimedia record.

- title:

  The title of the multimedia record.

- top:

  The number of pixels to omit from the top side of the image.

- left:

  The number of pixels to omit from the left side of the image.

- height:

  The height in pixels of the cropped region.

- width:

  The width in pixels of the cropped region.

## Value

An S7 object representing a GEDCOM MULTIMEDIA_LINK.

## Details

The properties @left and @top indicate the top left corner of the region
to display. The properties @width and @height indicate the dimensions of
the region to display.

If the multimedia record contains multiple files, then the crop
parameters only applies to the first file.

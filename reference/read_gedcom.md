# Import a GEDCOM file

Imports a \*.ged file and creates a gedcom object.

## Usage

``` r
read_gedcom(filepath = NULL, lines = NULL)
```

## Arguments

- filepath:

  The full filepath of the GEDCOM file. If this and the `lines`
  parameter are both NULL then you will be prompted to select a file.
  You may also provide the path to a GEDzip file (\*.zip), where there
  should be a single GEDCOM file located in the root directory of the
  archive.

- lines:

  If the filepath is not provided then a character vector of GEDCOM
  lines can be provided.

## Value

A gedcom S7 object.

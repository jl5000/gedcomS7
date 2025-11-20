# Delete potentially multiple structures from GEDCOM lines

Delete potentially multiple structures from GEDCOM lines

## Usage

``` r
delete_ged_sections(lines, line_fn, containing_line = TRUE)
```

## Arguments

- lines:

  A character vector of GEDCOM lines.

- line_fn:

  A callback function which takes a single input, lines, and returns an
  integer vector identifying the row of each structure to be deleted.

- containing_line:

  Whether the line number(s) returned by the callback are the first line
  of the structure(s) or whether the line number(s) reference a line
  within the structure (but not more than one level lower).

## Value

The character vector of GEDCOM lines without the structures identified
by the callback function.

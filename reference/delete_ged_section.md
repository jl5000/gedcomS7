# Delete a structure from GEDCOM lines

Delete a structure from GEDCOM lines

## Usage

``` r
delete_ged_section(lines, line_no, containing_line = TRUE)
```

## Arguments

- lines:

  A character vector of GEDCOM lines.

- line_no:

  A line number where the structure is located.

- containing_line:

  Whether the line number is the first line of the structure or whether
  the line number references a line within the structure (but not more
  than one level lower).

## Value

The character vector of GEDCOM lines without the structure referenced by
the line_no.

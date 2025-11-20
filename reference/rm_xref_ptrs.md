# Remove xref pointers from GEDCOM lines

Remove xref pointers from GEDCOM lines

## Usage

``` r
rm_xref_ptrs(lines, xref, void_refs)
```

## Arguments

- lines:

  A character vector of GEDCOM lines.

- xref:

  The xref to remove.

- void_refs:

  Whether to replace references to the xref with a @VOID@ reference, or
  remove the structure entirely.

## Value

The GEDCOM lines without pointers to the xref.

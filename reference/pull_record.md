# Pull a record from a GEDCOM object for editing

Pull a record from a GEDCOM object for editing

## Usage

``` r
pull_record(x, xref)
```

## Arguments

- x:

  A gedcom object.

- xref:

  The xref of the record to pull.

## Value

An S7 object representing the record.

## Details

The record is not removed from the gedcom object, rather a copy is
taken.

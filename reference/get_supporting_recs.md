# Identify all supporting records for a set of records

This function gets all supporting records (and onwards dependencies) for
a set of records. Supporting records are note, multimedia, source,
submitter and repository records, i.e. those providing supporting
evidence and comments.

## Usage

``` r
get_supporting_recs(
  x,
  xrefs,
  inc_note = TRUE,
  inc_media = TRUE,
  inc_sour = TRUE,
  inc_repo = TRUE,
  inc_subm = TRUE
)
```

## Arguments

- x:

  A gedcom object.

- xrefs:

  The xrefs of records to get supporting records for.

- inc_note:

  Whether to include Note records.

- inc_media:

  Whether to include Multimedia records.

- inc_sour:

  Whether to include Source records.

- inc_repo:

  Whether to include Repository records.

- inc_subm:

  Whether to include Submitter records.

## Value

A character vector of xrefs.

# Push an edited record back into a GEDCOM object

Push an edited record back into a GEDCOM object

## Usage

``` r
push_record(x, record)
```

## Arguments

- x:

  A gedcom object.

- record:

  An object representing the record to place back into the GEDCOM
  object.

## Value

An updated GEDCOM object.

## Details

The function will automatically keep family links for individuals
updated. It will also update the `@updated` property if
`@update_change_dates` in the gedcom object is set to TRUE.

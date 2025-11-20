# Save a gedcom object to disk as a GEDCOM file

Save a gedcom object to disk as a GEDCOM file

## Usage

``` r
write_gedcom(
  gedcom,
  filepath = file.choose(),
  inc_confid = TRUE,
  inc_private = TRUE,
  inc_living = TRUE
)
```

## Arguments

- gedcom:

  A gedcom object.

- filepath:

  The full filepath to write to.

- inc_confid:

  Whether to include records that are marked as confidential.

- inc_private:

  Whether to include records that are marked as private.

- inc_living:

  Whether to include individual records for suspected living people.

## Value

The filepath (invisibly).

## Details

This function prepares the gedcom object and then writes it to the
filepath. Steps taken include filtering sensitive data, escaping "@"
signs (with another "@") , and splitting long lines onto separate lines.

Writing to GEDzip files is not supported. If you want to do this then
write to a GEDCOM file first and then move the file to the GEDzip
archive.

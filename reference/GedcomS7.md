# Create a GEDCOM object

You shouldn't need to use this directly to create new GEDCOM objects.
Instead, use
[`new_gedcom()`](https://jl5000.github.io/gedcomS7/reference/new_gedcom.md)
which populates relevant defaults.

## Usage

``` r
GedcomS7(
  header = GedcomHeader(),
  records = GedcomRecords(),
  update_change_dates = FALSE,
  add_creation_dates = FALSE
)
```

## Arguments

- header:

  An S7 object whose properties contain information about the GEDCOM
  object as a whole.

- records:

  An S7 object whose properties contain information about all records.
  Do not edit properties in capitals directly. See Details for more
  information.

- update_change_dates:

  Whether to automatically update change dates when updating records.
  This happens when the record is pushed to the gedcom object.

- add_creation_dates:

  Whether to automatically add creation dates when creating records.
  This happens when the record is pushed to the gedcom object.

## Value

An S7 object representing a GEDCOM file.

## Details

All information about records is contained in the `@records` property.

The `@prefixes` property is a named vector containing any alphanumeric
string (up to 6 characters long) which will precede the number given to
identify new records, of which there are currently 7 types:

Individual (INDI) Family (FAM) Source (SOUR) Repository (REPO)
Multimedia (OBJE) Note (SNOTE) Submitter (SUBM)

This vector must be of a particular length with specific names. For
example: c(SUBM = "U", INDI = "I", FAM = "F", SOUR = "S", REPO = "R",
OBJE = "M", SNOTE = "N").

The order that these records appear in the vector will also dictate the
order in which records will appear in the exported file.

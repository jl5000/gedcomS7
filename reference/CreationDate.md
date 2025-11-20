# Create a creation date object

Create a creation date object

## Usage

    CreationDate(date_exact = <object>, time = character(0))

## Arguments

- date_exact:

  An exact date given either as a formatted GEDCOM string, or a
  [`DateExact()`](https://jl5000.github.io/gedcomS7/reference/DateExact.md)
  object. If not given, it will default to today's date.

- time:

  The time given either as a formatted GEDCOM string, or a
  [`Time()`](https://jl5000.github.io/gedcomS7/reference/Time.md)
  object.

## Value

An S7 object representing a GEDCOM CREATION_DATE.

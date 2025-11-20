# Create a change date object

Create a change date object

## Usage

    ChangeDate(
      date_exact = <object>,
      time = character(0),
      note_xrefs = character(0),
      notes = list()
    )

## Arguments

- date_exact:

  An exact date given either as a formatted GEDCOM string, or a
  [`DateExact()`](https://jl5000.github.io/gedcomS7/reference/DateExact.md)
  object. If not given, it will default to today's date.

- time:

  The time given either as a formatted GEDCOM string, or a
  [`Time()`](https://jl5000.github.io/gedcomS7/reference/Time.md)
  object.

- note_xrefs:

  A character vector of relevant note record cross-reference
  identifiers.

- notes:

  Associated notes. This can either be a
  [`Note()`](https://jl5000.github.io/gedcomS7/reference/Note.md)
  object, a list of them, or a character vector of notes.

## Value

An S7 object representing a GEDCOM CHANGE_DATE.

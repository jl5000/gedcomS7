# Create a GEDCOM header object

Create a GEDCOM header object

## Usage

    GedcomHeader(
      gedcom_version = character(0),
      ext_tags = character(0),
      source = NULL,
      destination = character(0),
      creation_date = <object>,
      creation_time = character(0),
      subm_xref = character(0),
      gedcom_copyright = character(0),
      default_language = character(0),
      default_place_form = character(0),
      notes = list(),
      note_xrefs = character(0)
    )

## Arguments

- gedcom_version:

  The version number of the official specification that this GEDCOM
  conforms to. This must include the major and minor version (for
  example, “7.0”); it may include the patch as well (for example,
  “7.0.1”), but doing so is not required.

- ext_tags:

  Not supported.

- source:

  A
  [`GedcomSource()`](https://jl5000.github.io/gedcomS7/reference/GedcomSource.md)
  object describing the software that has generated the GEDCOM.\`

- destination:

  An identifier for the system expected to receive this GEDCOM.

- creation_date:

  The creation date of the file given either as a formatted GEDCOM
  string, or a
  [`DateExact()`](https://jl5000.github.io/gedcomS7/reference/DateExact.md)
  object.

- creation_time:

  The creation time of the file given either as a formatted GEDCOM
  string, or a
  [`Time()`](https://jl5000.github.io/gedcomS7/reference/Time.md)
  object.

- subm_xref:

  The cross-reference identifier of the primary submitter.

- gedcom_copyright:

  A copyright statement, as appropriate for the copyright laws
  applicable to this data.

- default_language:

  The default language for the entire GEDCOM object.

- default_place_form:

  The default form for place names in the GEDCOM object. A
  comma-separated string of jurisdictional titles. For example "City,
  County, State, Country".

- notes:

  Associated notes. This can either be a
  [`Note()`](https://jl5000.github.io/gedcomS7/reference/Note.md)
  object, a list of them, or a character vector of notes.

- note_xrefs:

  A character vector of relevant note record cross-reference
  identifiers.

## Value

An S7 object representing a GEDCOM header.

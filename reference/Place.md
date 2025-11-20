# Create a place structure object

Create a place structure object

## Usage

``` r
Place(
  place_name = character(0),
  place_form = character(0),
  language = character(0),
  place_translations = character(0),
  lat_long = character(0),
  ext_ids = character(0),
  note_xrefs = character(0),
  notes = list()
)
```

## Arguments

- place_name:

  A comma-separated string of region names, ordered from smallest to
  largest. The specific meaning of each element is given by the
  @place_form, or in the `@default_place_form` of the gedcom object if
  there is no @place_form defined. Elements should be left blank if they
  are unknown, do not apply to the location, or are too specific for the
  region in question. For example "Baltimore, , Maryland, USA".

- place_form:

  A comma-separated string of jurisdictional titles, which has the same
  number of elements as @place_form. For example "City, County, State,
  Country".

- language:

  A character string of language tags as defined in BCP 47.

- place_translations:

  A named character vector of translations of the place name. The vector
  values must follow the same form as the @place_name and the vector
  names must be a language value as defined by @language.

- lat_long:

  The latitude and longitude of the place, separated by a space. The
  latitude coordinate is the direction North or South from the equator
  in degrees and fraction of degrees. The longitude coordinate is in
  degrees and fraction of degrees East or West of the zero or base
  meridian coordinate. For example: 18 degrees, 9 minutes, and 3.4
  seconds North, 168 degrees, 9 minutes, and 3.4 seconds East would be
  formatted as "N18.150944 E168.150944".

- ext_ids:

  A named character vector of identifiers maintained by an external
  authority. The names must be given as a URI. See the GEDCOM
  specification for more information.

- note_xrefs:

  A character vector of relevant note record cross-reference
  identifiers.

- notes:

  Associated notes. This can either be a
  [`Note()`](https://jl5000.github.io/gedcomS7/reference/Note.md)
  object, a list of them, or a character vector of notes.

## Value

An S7 object representing a GEDCOM PLACE_STRUCTURE.

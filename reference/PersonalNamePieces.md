# Create a name pieces object

Create a name pieces object

## Usage

``` r
PersonalNamePieces(
  prefix = character(0),
  given = character(0),
  nickname = character(0),
  surname_prefix = character(0),
  surname = character(0),
  suffix = character(0)
)
```

## Arguments

- prefix:

  The name prefix, e.g. Cmdr.

- given:

  The given name or earned name.

- nickname:

  A descriptive or familiar name that is used instead of, or in addition
  to, one’s proper name.

- surname_prefix:

  Surname prefix or article used in a family name. For example in the
  name "de la Cruz", this value would be "de la".

- surname:

  Surname or family name.

- suffix:

  Name piece that appears after the given name and surname parts, e.g.
  Jr.

## Value

An S7 object representing a GEDCOM PERSONAL_NAME_PIECES.

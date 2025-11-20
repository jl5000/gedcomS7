# Create a character-based property

Constructs a character property with optional constraints on size,
character length, pattern matching, and allowed choices.

## Usage

``` r
prop_char(
  min_size = NULL,
  max_size = NULL,
  min_char = NULL,
  max_char = NULL,
  choices = NULL,
  pattern = NULL,
  names_required = FALSE,
  default = NULL,
  casting_name = NULL,
  S7class_names = NULL
)
```

## Arguments

- min_size:

  Minimum number of elements allowed.

- max_size:

  Maximum number of elements allowed.

- min_char:

  Minimum number of characters per element.

- max_char:

  Maximum number of characters per element.

- choices:

  Character vector of allowed values.

- pattern:

  Regular expression pattern that values must match.

- names_required:

  Logical indicating whether property values should have names.

- default:

  Default value for the property.

- casting_name:

  The name of the property if you want to explicitly cast it to
  character type when being set (this allows users to provide other
  atomic types).

- S7class_names:

  Character vector of gedcomS7 class names that this property could also
  take.

## Value

An S7 property object.

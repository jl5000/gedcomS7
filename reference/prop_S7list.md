# Create a property for a list of gedcomS7 objects

Constructs a property that stores a list of objects of a specified
gedcomS7 class.

## Usage

``` r
prop_S7list(prop_name, S7_class)
```

## Arguments

- prop_name:

  A character string specifying the name of the property.

- S7_class:

  The gedcomS7 class that each element of the list should conform to.

## Value

An S7 property object.

## Details

If any elements of the list are of type character, then they will
automatically be converted into objects of the appropriate class. Any
elements that cannot be converted will result in the validator throwing
an error.

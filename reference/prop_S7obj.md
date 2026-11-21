# Create a property for a single gedcomS7 object

Constructs a property that can store a single object of a specified
gedcomS7 class.

## Usage

``` r
prop_S7obj(S7_class)
```

## Arguments

- S7_class:

  The gedcomS7 class that the property value should conform to.

## Value

An S7 property object.

## Details

The property will default to a value of NULL if no object is provided.

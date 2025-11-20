# Create a numeric property restricted to whole numbers

Constructs a numeric property with validation for whole numbers and
optional constraints on size and value range.

## Usage

``` r
prop_whole(min_size = NULL, max_size = NULL, min_val = NULL, max_val = NULL)
```

## Arguments

- min_size:

  Minimum number of elements allowed.

- max_size:

  Maximum number of elements allowed.

- min_val:

  Minimum numeric value allowed.

- max_val:

  Maximum numeric value allowed.

## Value

An S7 property object.

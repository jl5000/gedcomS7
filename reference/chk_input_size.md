# Validate the size of an input

Validate the size of an input

## Usage

``` r
chk_input_size(
  input,
  min_len = NULL,
  max_len = NULL,
  min_val = NULL,
  max_val = NULL
)
```

## Arguments

- input:

  The input.

- min_len:

  The minimum number of elements the input should have.

- max_len:

  The maximum number of elements the input should have.

- min_val:

  The minimum number of characters or value the input should have
  (depending on input type).

- max_val:

  The maximum number of characters or value the input should have
  (depending on input type).

## Value

Either a character string giving an error message, or NULL.

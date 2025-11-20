# Validate the existence of a subordinate phrase

Validate the existence of a subordinate phrase

## Usage

``` r
chk_input_phrase(input, name, parent, parent_name, parent_val)
```

## Arguments

- input:

  The value of the phrase input.

- name:

  The phrase input name.

- parent:

  The parent input.

- parent_name:

  The name of the parent input used in any error messages.

- parent_val:

  The value of the parent input which requires a subordinate phrase.

## Value

Either a character string giving an error message, or NULL.

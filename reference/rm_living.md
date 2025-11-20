# Remove living individuals in a GEDCOM object

Remove living individuals in a GEDCOM object

## Usage

``` r
rm_living(x, max_age = 100)
```

## Arguments

- x:

  A gedcom object.

- max_age:

  The maximum age to assume for a living person (if a date of birth is
  given).

## Value

A gedcom object cleansed of information on living individuals.

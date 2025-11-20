# Identify all parents for an individual

Identify all parents for an individual

## Usage

``` r
get_indi_parents(x, xref, pedigrees = NULL)
```

## Arguments

- x:

  A gedcom object.

- xref:

  The xref of an Individual record.

- pedigrees:

  A character vector of allowed family-child linkages. By default, NULL
  means all pedigrees (e.g. inc. ADOPTED). If it includes "BIRTH" then
  this will also pick up non-existent values (since BIRTH is assumed to
  be the default).

## Value

A character vector of xrefs.

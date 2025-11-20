# Identify all children in a family

Identify all children in a family

## Usage

``` r
get_fam_children(x, xref, pedigrees = NULL)
```

## Arguments

- x:

  A gedcom object.

- xref:

  The xref of a Family record.

- pedigrees:

  A character vector of allowed family-child linkages. By default, NULL
  means all pedigrees (e.g. inc. ADOPTED). If it includes "BIRTH" then
  this will also pick up non-existent values (since BIRTH is assumed to
  be the default).

## Value

A character vector of xrefs.

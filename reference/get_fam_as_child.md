# Identify all families for an individual where they are a child

Identify all families for an individual where they are a child

## Usage

``` r
get_fam_as_child(x, xref, pedigrees = NULL)
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

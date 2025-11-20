# Identify all descendants for an individual

This function identifies records in an entire branch of the family tree
below a certain individual.

## Usage

``` r
get_descendants(
  x,
  xref,
  inc_indi = FALSE,
  inc_part = FALSE,
  inc_fam = FALSE,
  inc_supp = FALSE,
  pedigrees = NULL
)
```

## Arguments

- x:

  A gedcom object.

- xref:

  The xref of an Individual record.

- inc_indi:

  Whether to also include the individual themselves.

- inc_part:

  Whether to also include all partners of this individual (and their
  descendants and descendants' partners).

- inc_fam:

  Whether to also include all Family records where this individual is a
  partner (and all descendants' Family records).

- inc_supp:

  Whether to also include all supporting records (Note, Source,
  Repository, Multimedia).

- pedigrees:

  A character vector of allowed family-child linkages. By default, NULL
  means all pedigrees (e.g. inc. ADOPTED). If it includes "BIRTH" then
  this will also pick up non-existent values (since BIRTH is assumed to
  be the default).

## Value

A character vector of xrefs.

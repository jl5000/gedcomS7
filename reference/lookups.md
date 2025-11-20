# Lookup values

Lookup values

## Usage

``` r
val_record_types()

val_adoptive_parents()

val_individual_attribute_types(inc_generic = FALSE)

val_individual_event_types(inc_generic = FALSE)

val_family_event_types(inc_generic = FALSE)

val_family_attribute_types(inc_generic = FALSE)

val_event_types(inc_generic = FALSE)

val_attribute_types(inc_generic = FALSE)

val_fact_types(inc_generic = FALSE)

val_individual_ordinance_types()

val_family_ordinance_types()

val_ordinance_states(ord_type)

val_medium_types()

val_pedigree_types()

val_certainty()

val_restriction()

val_roles()

val_sexes()

val_confidence_types()

val_name_types()
```

## Arguments

- inc_generic:

  Whether to include a generic facts.

- ord_type:

  One of the values in `val_individual_ordinance_types()` or
  `val_family_ordinance_types()`.

## Value

A vector of allowed values.

# Create an address object

Create an address object

## Usage

``` r
Address(
  full = character(0),
  city = character(0),
  state = character(0),
  postal_code = character(0),
  country = character(0),
  adr1 = character(0),
  adr2 = character(0),
  adr3 = character(0)
)
```

## Arguments

- full:

  A full address as it would appear on a mailing label, with lines
  separated by semi-colon and a space. For example: "The White House;
  1600 Pennsylvania Avenue N.W.; Washington, DC 20500; United States of
  America"

- city:

  The city component of the address.

- state:

  The state component of the address.

- postal_code:

  The postal code component of the address.

- country:

  The country component of the address.

- adr1:

  Deprecated.

- adr2:

  Deprecated.

- adr3:

  Deprecated.

## Value

An S7 object representing a GEDCOM ADDRESS_STRUCTURE.

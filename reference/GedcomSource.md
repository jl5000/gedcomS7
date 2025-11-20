# Create a GEDCOM source object

Create a GEDCOM source object

## Usage

``` r
GedcomSource(
  product_id = character(0),
  product_name = character(0),
  product_version = character(0),
  business_name = character(0),
  business_address = NULL,
  phone_numbers = character(0),
  emails = character(0),
  faxes = character(0),
  web_pages = character(0),
  data_name = character(0),
  data_pubdate = character(0),
  data_pubtime = character(0),
  data_copyright = character(0)
)
```

## Arguments

- product_id:

  An identifier for the product producing this dataset.

- product_name:

  The name of the product producing this dataset.

- product_version:

  The version of the product producing this dataset.

- business_name:

  The name of the business, corporation, or person that produced or
  commissioned the product.

- business_address:

  The address of the business, corporation, or person that produced or
  commissioned the product. The address is given either as a
  [`Address()`](https://jl5000.github.io/gedcomS7/reference/Address.md)
  object or as a character string. This would be as written on a mailing
  label with new lines separated by \n.

- phone_numbers:

  A character vector of phone numbers.

- emails:

  A character vector of email addresses.

- faxes:

  A character vector of fax numbers.

- web_pages:

  A character vector of web page URLs.

- data_name:

  Deprecated.

- data_pubdate:

  Deprecated.

- data_pubtime:

  Deprecated.

- data_copyright:

  Deprecated.

## Value

An S7 object representing a GEDCOM HEAD.SOUR.

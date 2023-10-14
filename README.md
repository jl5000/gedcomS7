
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gedcomS7

<!-- badges: start -->
<!-- badges: end -->

The goal of `gedcomS7` is to handle genealogical data as GEDCOM files.
This package succeeds the `tidyged` package and many of the other
packages in the `gedcompendium`. The key changes are:

- The package is designed to work with GEDCOM 7.1, rather than 5.5.5.
- The package has minimal dependencies; currently only two.
- GEDCOM files are stored as S7 objects rather than dataframes.
- The package contains all functionality to create, edit, and
  interrogate GEDCOM files.

> This package is still in development. Be wary of editing your file
> with it for the time being.

## Installation

You can install the development version of gedcomS7 from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("jl5000/gedcomS7")
```

## Key limitations

The package does not currently support extension tags. If you import a
file with these features, the package may behave unpredictably.

## Import / creation

The intent is to allow the user to import GEDCOM files or create them
from scratch, creating an S7 object representation:

``` r
library(gedcomS7)
#> When importing existing GEDCOM files, you should ensure that they are error free.
#> This package assumes imported GEDCOM files are valid and very few validation checks are carried out.
#> Several GEDCOM validators are available, including an online validator at https://ged-inline.org/

ged <- read_gedcom(system.file("extdata", "maximal70.ged", package = "gedcomS7"))
ged
#> GEDCOM file summary:
#>  
#> 
#> Copyright:              another copyright statement
#> 
#> 
#> Submitters:             2
#> Individuals:            4
#> Families:               2
#> Sources:                2
#> Repositories:           2
#> Multimedia:             2
#> Notes:                  2

ged <- new_gedcom()
ged
#> GEDCOM file summary:
#>  
#> 
#> 
#> 
#> Submitters:             0
#> Individuals:            0
#> Families:               0
#> Sources:                0
#> Repositories:           0
#> Multimedia:             0
#> Notes:                  0
```

Once you have created or imported your file, you can set various high
level options for your file.

## Options

You can influence how the gedcom object is modified as you add and edit
records within it.

The property `ged@xref_prefixes` is a named vector containing any
alphanumeric string (up to 6 characters long) which will precede the
number given to identify new records (of which there are 7 types). This
vector must be of a particular length with these specific names.

``` r
ged@xref_prefixes
#>  subm  indi   fam  sour  repo media  note 
#>   "U"   "I"   "F"   "S"   "R"   "M"   "N"
```

The order that these records appear in the vector will also dictate the
order in which records will appear in the exported file.

Since we have no existing records in our gedcom object, the next xrefs
of each type will be:

``` r
ged@next_xref
#>   subm   indi    fam   sour   repo  media   note 
#> "@U1@" "@I1@" "@F1@" "@S1@" "@R1@" "@M1@" "@N1@"
```

Note: xrefs are not supposed to be exposed to the typical user…

You can also use the `add_creation_dates` and `update_change_dates`
properties to add or update the date a record was created/edited (they
don’t by default as it can bloat files considerably).

``` r
ged@add_creation_dates <- TRUE
ged@update_change_dates <- TRUE
```

## GEDCOM metadata

The metadata for the gedcom object (ged@…) can be modified.

``` r
ged@notes <- "This file contains my family tree"
ged@gedcom_copyright <- "Copyright Jamie Lendrum 2023"
ged
#> GEDCOM file summary:
#>  
#> 
#> Copyright:              Copyright Jamie Lendrum 2023
#> 
#> 
#> Submitters:             0
#> Individuals:            0
#> Families:               0
#> Sources:                0
#> Repositories:           0
#> Multimedia:             0
#> Notes:                  0
```

## New records

New records are created using the set of `class_record_*()` functions:

``` r
luke <- class_record_indi(
  sex = "M",
  pers_names = "Luke /Skywalker/"
)
```

An individual can have many names, which are created using
`class_personal_name()` and `class_name_info()` and must be stored in a
list:

## The push/pull paradigm

New record objects are added to an existing gedcom by using
`push_record()`:

``` r
ged <- push_record(ged, luke)
#> New Individual record added with xref @I1@
ged
#> GEDCOM file summary:
#>  
#> 
#> Copyright:              Copyright Jamie Lendrum 2023
#> 
#> 
#> Submitters:             0
#> Individuals:            1
#> Families:               0
#> Sources:                0
#> Repositories:           0
#> Multimedia:             0
#> Notes:                  0
```

Once in the gedcom object, they are stored as raw gedcom data as a
character vector:

``` r
ged@indi[[1]]
#> [1] "0 @I1@ INDI"             "1 NAME Luke /Skywalker/"
#> [3] "1 SEX M"                 "1 CHAN"                 
#> [5] "2 DATE 14 OCT 2023"      "1 CREA"                 
#> [7] "2 DATE 14 OCT 2023"
```

If you want to edit a record, you must pull it from the GEDCOM object,
which will take a copy and parse it into an editable S7 object:

``` r
pull_record(ged, "@I1@")
#> 0 @I1@ INDI

#> 1 NAME Luke /Skywalker/

#> 1 SEX M

#> 1 CHAN

#> 2 DATE 14 OCT 2023

#> 1 CREA

#> 2 DATE 14 OCT 2023
```

## References

## Notice

> This work comprises, is based on, or is derived from the FAMILYSEARCH
> GEDCOM™ Specification, © 1984-2023 Intellectual Reserve, Inc. All
> rights reserved.

> “FAMILYSEARCH GEDCOM™” and “FAMILYSEARCH®” are trademarks of
> Intellectual Reserve, Inc. and may not be used except as allowed by
> the Apache 2.0 license that governs this work or as expressly
> authorized in writing and in advance by Intellectual Reserve, Inc.

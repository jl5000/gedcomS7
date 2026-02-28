# Creation of GEDCOM objects

## Creating new objects

The easiest way to create a GEDCOM object is to import an existing
GEDCOM file. The example below imports the smallest legal GEDCOM file:

``` r
library(gedcomS7)

ged <- read_gedcom("minimal70.ged")

ged
#> GEDCOM file summary:
#>  
#> GEDCOM version:     7.0
#> Creation Date:      <Undefined>
#> Default Language:   <Undefined>
#> Source system:      <Undefined>
#> 
#> Copyright:          <Undefined>
#> 
#> Submitters:         0
#> Individuals:        0
#> Families:           0
#> Sources:            0
#> Repositories:       0
#> Multimedia:         0
#> Notes:              0
```

A GEDCOM object can also be created using the
[`new_gedcom()`](https://jl5000.github.io/gedcomS7/reference/new_gedcom.md)
function:

``` r
ged_new <- new_gedcom()

ged_new
#> GEDCOM file summary:
#>  
#> GEDCOM version:     7.0
#> Creation Date:      28 FEB 2026
#> Default Language:   en
#> Source:             gedcomS7
#> Source name:        The 'gedcomS7' package for the R language
#> 
#> Copyright:          <Undefined>
#> 
#> Submitters:         0
#> Individuals:        0
#> Families:           0
#> Sources:            0
#> Repositories:       0
#> Multimedia:         0
#> Notes:              0
```

Information about the `gedcomS7` package (as the system/product creating
the file) is given in the appropriate places, creation date, as well as
a default language (English). The default language can be changed in the
[`new_gedcom()`](https://jl5000.github.io/gedcomS7/reference/new_gedcom.md)
function. This must be a character string of language tags as defined in
[BCP 47](https://www.rfc-editor.org/info/bcp47) (as must any other
language parameter in the package).

Once you have a GEDCOM object in R, you can write it to a valid GEDCOM
file using
[`write_gedcom()`](https://jl5000.github.io/gedcomS7/reference/write_gedcom.md).
The filepath must end with file extension .ged.

## GEDCOM object properties

The properties of a GEDCOM object are:

``` r
str(ged, max.level = 1)
#> <gedcomS7::GedcomS7>
#>  @ header             : <gedcomS7::GedcomHeader>
#>  .. @ gedcom_version    : chr "7.0"
#>  .. @ ext_tags          : chr(0) 
#>  .. @ source            : NULL
#>  .. @ destination       : chr(0) 
#>  .. @ creation_date     : chr(0) 
#>  .. @ creation_time     : chr(0) 
#>  .. @ subm_xref         : chr(0) 
#>  .. @ gedcom_copyright  : chr(0) 
#>  .. @ default_language  : chr(0) 
#>  .. @ default_place_form: chr(0) 
#>  .. @ notes             : list()
#>  .. @ note_xrefs        : chr(0) 
#>  .. @ GEDCOM            : chr [1:3] "0 HEAD" "1 GEDC" "2 VERS 7.0"
#>  @ records            : <gedcomS7::GedcomRecords>
#>  .. @ prefixes    : Named chr [1:7] "U" "I" "F" "S" "R" "M" "N"
#>  .. .. - attr(*, "names")= chr [1:7] "SUBM" "INDI" "FAM" "SOUR" ...
#>  .. @ XREFS       :List of 7
#>  .. @ XREFS_PRIV  :List of 7
#>  .. @ XREFS_CONFID:List of 7
#>  .. @ XREFS_NEXT  : Named chr [1:7] "@U1@" "@I1@" "@F1@" "@S1@" "@R1@" "@M1@" "@N1@"
#>  .. .. - attr(*, "names")= chr [1:7] "SUBM" "INDI" "FAM" "SOUR" ...
#>  .. @ RAW         : <gedcomS7::GedcomRecordsRaw>
#>  .. .. @ SUBM : Named list()
#>  .. .. @ INDI : Named list()
#>  .. .. @ FAM  : Named list()
#>  .. .. @ SOUR : Named list()
#>  .. .. @ REPO : Named list()
#>  .. .. @ OBJE : Named list()
#>  .. .. @ SNOTE: Named list()
#>  @ update_change_dates: logi FALSE
#>  @ add_creation_dates : logi FALSE
#>  @ GEDCOM             : chr [1:4] "0 HEAD" "1 GEDC" "2 VERS 7.0" "0 TRLR"
```

The properties of the main GEDCOM object are either:

- Properties of the header in the `@header` property (metadata about the
  file as a whole).
- Information about records in the `@records` property. This is composed
  of:
  - Properties that summarise and control the cross-reference
    identifiers (xrefs) given to records (e.g. `@XREFS`, `@prefixes`,
    and `@XREFS_NEXT`);
  - Lists of records of each type (`@RAW`);
- Those that allow the user to control whether creation/change dates are
  added to records as they are pushed to the GEDCOM object
  (`@update_change_dates` and `@add_creation_dates`) - they don’t by
  default as it can bloat files considerably;
- Those that show you what the GEDCOM file representation looks like
  (`@GEDCOM`).

Some of these properties are also read-only, which means they are
calculated from other properties and cannot be directly set. These are
given in capitals.

# Implementation of gedcomS7

## Why gedcomS7?

One of the main characteristics I wanted for this package was to hide
the complexity of the GEDCOM specification, and try to automate
genealogical tasks that are time consuming to do manually.

I spent a significant amount of time before writing any code considering
my options for how the data would be stored under the hood. In one blog
I considered storing genealogical data in a relational table format as
it is easier to deal with, but discounted it very quickly as it is not
well suited to nested data (and list columns are not easy to deal with).

I toyed with the idea of using an off-the-shelf open source product like
[GRAMPS](https://gramps-project.org) but I found it awkward to use and
wanted something where I was in complete control, taking full advantage
of the strengths of R.

I also considered using data structures more suited to this type of
data, such as JSON or graphs (using the `igraph` or `data.tree`
package). However, I discovered it would be quite difficult representing
some of the structures in the GEDCOM specification to my satisfaction.

My preference was for an Object Orientated approach, which is what many
other applications use, but none of the OOP solutions in R quite fit the
bill.

My first serious attempt at doing this resulted in `tidyged` and the
other packages of the `gedcompendium`. This adopted a dataframe and
`tidyverse` approach and whilst it did work it was dependency-heavy and
required a lot of processing under the hood.

The release of `S7` presented an ideal opportunity to revisit creating
an OOP-based GEDCOM package in R. Initial testing had promising results
and so I continued building out the rest of the package, using the
updated GEDCOM 7.0 specification.

## The gedcomS7 object

The main GEDCOM object is an S7 representation of a GEDCOM file:

``` r
library(gedcomS7)

ged <- read_gedcom("remarriage1.ged")

str(ged)
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
#>  .. .. $ SUBM : chr(0) 
#>  .. .. $ INDI : chr [1:3] "@I1@" "@I2@" "@I3@"
#>  .. .. $ FAM  : chr [1:2] "@F1@" "@F2@"
#>  .. .. $ SOUR : chr(0) 
#>  .. .. $ REPO : chr(0) 
#>  .. .. $ OBJE : chr(0) 
#>  .. .. $ SNOTE: chr(0) 
#>  .. @ XREFS_PRIV  :List of 7
#>  .. .. $ SUBM : chr(0) 
#>  .. .. $ INDI : chr(0) 
#>  .. .. $ FAM  : chr(0) 
#>  .. .. $ SOUR : chr(0) 
#>  .. .. $ REPO : chr(0) 
#>  .. .. $ OBJE : chr(0) 
#>  .. .. $ SNOTE: chr(0) 
#>  .. @ XREFS_CONFID:List of 7
#>  .. .. $ SUBM : chr(0) 
#>  .. .. $ INDI : chr(0) 
#>  .. .. $ FAM  : chr(0) 
#>  .. .. $ SOUR : chr(0) 
#>  .. .. $ REPO : chr(0) 
#>  .. .. $ OBJE : chr(0) 
#>  .. .. $ SNOTE: chr(0) 
#>  .. @ XREFS_NEXT  : Named chr [1:7] "@U1@" "@I4@" "@F3@" "@S1@" "@R1@" "@M1@" "@N1@"
#>  .. .. - attr(*, "names")= chr [1:7] "SUBM" "INDI" "FAM" "SOUR" ...
#>  .. @ RAW         : <gedcomS7::GedcomRecordsRaw>
#>  .. .. @ SUBM : Named list()
#>  .. .. @ INDI :List of 3
#>  .. .. .. $ @I1@: chr [1:5] "0 @I1@ INDI" "1 NAME John Q /Public/" "1 SEX M" "1 FAMS @F1@" ...
#>  .. .. .. $ @I2@: chr [1:4] "0 @I2@ INDI" "1 NAME Jane /Doe/" "1 SEX F" "1 FAMS @F1@"
#>  .. .. .. $ @I3@: chr [1:5] "0 @I3@ INDI" "1 NAME Mary /Roe/" "1 DEAT" "2 DATE 1 MAR 1914" ...
#>  .. .. @ FAM  :List of 2
#>  .. .. .. $ @F1@: chr [1:9] "0 @F1@ FAM" "1 HUSB @I1@" "1 WIFE @I2@" "1 MARR" ...
#>  .. .. .. $ @F2@: chr [1:5] "0 @F2@ FAM" "1 HUSB @I1@" "1 WIFE @I3@" "1 MARR" ...
#>  .. .. @ SOUR : Named list()
#>  .. .. @ REPO : Named list()
#>  .. .. @ OBJE : Named list()
#>  .. .. @ SNOTE: Named list()
#>  @ update_change_dates: logi FALSE
#>  @ add_creation_dates : logi FALSE
#>  @ GEDCOM             : chr [1:32] "0 HEAD" "1 GEDC" "2 VERS 7.0" "0 @I1@ INDI" ...
```

Properties of the GEDCOM object can be accessed and modified using the
`@` operator, e.g.

``` r
ged@header@default_language
#> character(0)
ged@header@default_language <- "en"
```

This ease of modification of specific properties of a GEDCOM object
wasn’t possible with the `tidyged` package.

Properties which don’t have values are either empty vectors, empty
lists, or NULL (depending on the property). Many properties take values
which are particular `gedcomS7` objects (or lists of them, if they take
more than one). For ease of use, you are often permitted to provide a
simple atomic vector, and `gedcomS7` will convert these to their
relevant objects automatically, but only if the object only requires its
first property. For example, the `@notes` property can take a simple
character vector of notes, and these will be converted to a list of
[`Note()`](https://jl5000.github.io/gedcomS7/reference/Note.md) objects.

``` r
ged@header@notes <- c("This is a note", "This is another note")
str(ged@header@notes)
#> List of 2
#>  $ : <gedcomS7::Note>
#>   ..@ text        : chr "This is a note"
#>   ..@ language    : chr(0) 
#>   ..@ media_type  : chr(0) 
#>   ..@ translations: list()
#>   ..@ citations   : list()
#>   ..@ GEDCOM      : chr "0 NOTE This is a note"
#>  $ : <gedcomS7::Note>
#>   ..@ text        : chr "This is another note"
#>   ..@ language    : chr(0) 
#>   ..@ media_type  : chr(0) 
#>   ..@ translations: list()
#>   ..@ citations   : list()
#>   ..@ GEDCOM      : chr "0 NOTE This is another note"
```

You can then access properties of these:

``` r
ged@header@notes[[2]]@language <- "en"
```

Some of these properties are also read-only (calculated from other
properties), such as `@GEDCOM` and these are indicated by being in all
capitals. The exploration of all properties of the GEDCOM object is
beyond the scope of this article, however the implementation of how
properties are stored is important.

## The Push/Pull paradigm

A GEDCOM file could contain many thousands of records containing
information on individuals, families, notes, sources, etc. Whilst
storing each of these records as `S7` objects within the main GEDCOM
object is theoretically possible, in practice it very quickly eats up
too much memory rendering the idea a non-starter.

For this reason, records are stored in their raw form from the GEDCOM
file as lists of character vectors in the `@RAW` property of the
`@records` property. For example, the lines in the GEDCOM file for the
first individual are:

``` r
ged@records@RAW@INDI[[1]]
#> [1] "0 @I1@ INDI"            "1 NAME John Q /Public/" "1 SEX M"               
#> [4] "1 FAMS @F1@"            "1 FAMS @F2@"
```

You can also reference records by their xref:

``` r
ged@records@RAW@INDI[["@I1@"]]
#> [1] "0 @I1@ INDI"            "1 NAME John Q /Public/" "1 SEX M"               
#> [4] "1 FAMS @F1@"            "1 FAMS @F2@"
```

If you want to edit a record, you must first Pull it from the GEDCOM
object. This takes a copy of the record and parses it into an editable
`S7` object.

``` r
john_public <- pull_record(ged, "@I1@")

str(john_public, max.level = 1)
#> <gedcomS7::IndividualRecord>
#>  @ XREF          : chr "@I1@"
#>  @ confidential  : logi FALSE
#>  @ locked        : logi FALSE
#>  @ private       : logi FALSE
#>  @ user_ids      : chr(0) 
#>  @ unique_ids    : chr(0) 
#>  @ ext_ids       : chr(0) 
#>  @ note_xrefs    : chr(0) 
#>  @ notes         : list()
#>  @ citations     : list()
#>  @ media_links   : list()
#>  @ created       : NULL
#>  @ updated       : NULL
#>  @ pers_names    :List of 1
#>  @ sex           : chr "M"
#>  @ facts         : list()
#>  @ non_events    : list()
#>  @ ordinances    : list()
#>  @ fam_links_chil: list()
#>  @ fam_links_spou:List of 2
#>  @ subm_xrefs    : chr(0) 
#>  @ associations  : list()
#>  @ alia_xrefs    : chr(0) 
#>  @ anci_xrefs    : chr(0) 
#>  @ desi_xrefs    : chr(0) 
#>  @ PRIMARY_NAME  : chr "John Q Public"
#>  @ ALL_NAMES     : chr "John Q Public"
#>  @ BIRTH_DATE    : chr(0) 
#>  @ BIRTH_PLACE   : chr(0) 
#>  @ IS_ALIVE      : logi TRUE
#>  @ DEATH_DATE    : chr(0) 
#>  @ DEATH_PLACE   : chr(0) 
#>  @ GEDCOM        : chr [1:5] "0 @I1@ INDI" "1 NAME John Q /Public/" "1 SEX M" ...
```

We can edit a property and Push it back to the GEDCOM object:

``` r
john_public@notes <- "John once had a dog called Rover"

ged <- push_record(ged, john_public)

ged@records@RAW@INDI[[1]]
#> [1] "0 @I1@ INDI"                            
#> [2] "1 NAME John Q /Public/"                 
#> [3] "1 SEX M"                                
#> [4] "1 FAMS @F1@"                            
#> [5] "1 FAMS @F2@"                            
#> [6] "1 NOTE John once had a dog called Rover"
```

You should never attempt to modify the records in their character vector
form directly from the GEDCOM object - additional checks and automated
tasks are carried out to ensure self-consistency during the Push
process.

Removing records can be achieved with
[`rm_records()`](https://jl5000.github.io/gedcomS7/reference/rm_records.md).
By default, any references to this record from other records will be
replaced with a “@VOID@” reference. If this is not chosen, then
references will be removed altogether (potentially losing supplementary
information).

## Viewing `gedcomS7` objects

There are two ways to view the contents of a `gedcomS7` object - the
most comprehensive is to use [`str()`](https://rdrr.io/r/utils/str.html)
(as above) which will show you every property the object has. The
alternative is to use [`print()`](https://rdrr.io/r/base/print.html) or
[`summary()`](https://rdrr.io/r/base/summary.html), both of which will
provide a brief summary of the object (not every property may be
displayed). For example:

``` r
ged
#> GEDCOM file summary:
#>  
#> GEDCOM version:     7.0
#> Creation Date:      <Undefined>
#> Default Language:   en
#> Source system:      <Undefined>
#> 
#> Copyright:          <Undefined>
#> 
#> Submitters:         0
#> Individuals:        3
#> Families:           2
#> Sources:            0
#> Repositories:       0
#> Multimedia:         0
#> Notes:              0
ged@header@notes
#> [[1]]
#> Note:           This is a note
#> 
#> Language:       <Undefined>
#> Format:         <Undefined>
#> Translations:   0
#> Citations:      0
#> 
#> [[2]]
#> Note:           This is another note
#> 
#> Language:       en
#> Format:         <Undefined>
#> Translations:   0
#> Citations:      0
john_public@pers_names
#> [[1]]
#> Personal Name:   John Q /Public/
#> Name Type:       <Undefined>
#> 
#> Translations:    0
#> Citations:       0
#> Notes:           0
```

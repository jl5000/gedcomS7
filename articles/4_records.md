# Records

## Introduction

This article introduces records, which are how genealogical data is
stored and organised in a GEDCOM file. There are several types of
records, which are the subject of subsequent articles, but this article
focuses on those aspects which they all (or almost all) have in common.

New records are created using the set of `*Record()` functions, e.g.

``` r
library(gedcomS7)

indi <- IndividualRecord()
```

## Restrictions

Records are considered read-only when the `@locked` property is set to
TRUE. If you attempt to pull a locked record from a GEDCOM object for
editing, you will be presented with a warning:

``` r
indi@locked <- TRUE
ged <- push_record(new_gedcom(), indi)
#> New Individual record added with xref @I1@
indi <- pull_record(ged, "@I1@")
#> Warning in check_restrictions(rec): The record is locked. Ensure you have the
#> record owner's permission before editing it and pushing it back to the GEDCOM
#> object.
```

There are two other properties that place restrictions on records:
`@confidential` and `@private`. The exact interpretation of confidential
and private is largely up to the author of the file, but they allow two
independent mechanisms for excluding certain records on export.

## Identifiers

### Cross references

All GEDCOM records are given unique identifiers known as xrefs
(cross-references) to allow other records to link to them. These are
alphanumeric strings surrounded by ‘@’ symbols.

Even though xref identifiers will be imported unchanged in the
`gedcomS7` package, some systems do create their own xref identifiers on
import. So you cannot assume they will survive between systems. However,
they should always be internally consistent.

For this reason xref identifiers are not supposed to be exposed to the
typical user. However this rule can only really be applied to GEDCOM
software that has a point-and-click user interface, rather than one that
works interactively at the R console (and the `S7` package does not
allow you to hide property values). If a `shiny` app is created, then
xrefs will be hidden from the user.

### Summarising and controlling xrefs

The `gedcomS7` package creates xrefs automatically when creating and
pushing new records. When creating a new record it will be given an xref
identifying it as a standalone record that has not yet been pushed to
the GEDCOM object:

``` r
new_person <- IndividualRecord()
new_person@XREF
#> [1] "@GEDCOMS7_ORPHAN@"
```

This is a special xref which indicates to the code that this is a new
record and not an existing one. It is important you do not change it.

If you then push it to a GEDCOM object, it will assign it a proper xref:

``` r
ged <- push_record(new_gedcom(), new_person)
#> New Individual record added with xref @I1@
```

The property `@records@prefixes` is a named vector containing any
alphanumeric string (up to 6 characters long) which will precede the
number given to identify new records (of which there are 7 types). This
vector must be of a particular length with these specific names.

We’ll import a different GEDCOM file which has some records in it:

``` r
ged_max <- read_gedcom("maximal70.ged")

ged_max@records@prefixes
#>  SUBM  INDI   FAM  SOUR  REPO  OBJE SNOTE 
#>   "U"   "I"   "F"   "S"   "R"   "M"   "N"
```

The order that these records appear in the vector will also dictate the
order in which records will appear in the exported file.

The `@records@XREFS` property gives a list of record xrefs in the GEDCOM
object, split by record type:

``` r
ged_max@records@XREFS
#> $SUBM
#> [1] "@U1@" "@U2@"
#> 
#> $INDI
#> [1] "@I1@" "@I2@" "@I3@" "@I4@"
#> 
#> $FAM
#> [1] "@F1@" "@F2@"
#> 
#> $SOUR
#> [1] "@S1@" "@S2@"
#> 
#> $REPO
#> [1] "@R1@" "@R2@"
#> 
#> $OBJE
#> [1] "@O1@" "@O2@"
#> 
#> $SNOTE
#> [1] "@N1@" "@N2@"
```

The next xrefs of each type will therefore be:

``` r
ged_max@records@XREFS_NEXT
#>   SUBM   INDI    FAM   SOUR   REPO   OBJE  SNOTE 
#> "@U3@" "@I5@" "@F3@" "@S3@" "@R3@" "@M1@" "@N3@"
```

### Other identifiers

As well as cross-reference identifiers, which are internally defined,
there are also a number of other identifiers that can be supplied to a
record:

- User-defined identifiers (`@user_ids`)
- Globally unique identifiers (`@unique_ids`)
- Identifiers given by an external authority (`@ext_ids`)

The `@user_ids` must be a vector of user reference numbers, for example
it may be a record number within the submitter’s automated or manual
system, or it may be a page and position number on a pedigree chart. It
can optionally be a named vector, where the vector names describe what
the reference number is. It’s usually a good idea to provide this.

The `@unique_ids` must take the form of a [Universally unique
identifier](https://en.wikipedia.org/wiki/Universally_unique_identifier)
(UUID). These can be generated with
[`uuid::UUIDgenerate()`](https://rdrr.io/pkg/uuid/man/UUIDgenerate.html),
e.g.

``` r
uuid::UUIDgenerate(n = 1)
#> [1] "de7a4126-252a-4709-9a6f-df23878cc9cc"
```

The `@ext_ids` must take the form of a named vector where the names are
the URI defining the identifier. For example, to include the reference
to an individual’s [Find a Grave’s](https://www.findagrave.com) page,
you would supply `c("https://www.findagrave.com/memorial/" = "1075")`,
which would be interpreted as
<https://www.findagrave.com/memorial/1075>.

## Referencing other records

One of the most important aspects of a record is the provenance of the
data within it. This can be provided via linking it with evidence
(sources) and multimedia. It should be noted that all of these linkages
can not only be provided at the record level, but also at more granular
levels; for example, you can provide source citations for each personal
name for an individual.

### Source citations

Linkages to Source records (known as source citations) are among the
most important aspects of a GEDCOM file. They are accessed via the
`@citations` property. This takes a list of
[`SourceCitation()`](https://jl5000.github.io/gedcomS7/reference/SourceCitation.md)
objects. You can provide a single object, or even a character vector of
Source record xrefs, and it will be converted into a list of
[`SourceCitation()`](https://jl5000.github.io/gedcomS7/reference/SourceCitation.md)
objects.

``` r
SourceCitation() |> 
  str()
#> <gedcomS7::SourceCitation>
#>  @ sour_xref  : chr "@VOID@"
#>  @ where      : chr(0) 
#>  @ date       : chr(0) 
#>  @ source_text: list()
#>  @ fact_type  : chr(0) 
#>  @ fact_phrase: chr(0) 
#>  @ role       : chr(0) 
#>  @ role_phrase: chr(0) 
#>  @ certainty  : chr(0) 
#>  @ media_links: list()
#>  @ note_xrefs : chr(0) 
#>  @ notes      : list()
#>  @ GEDCOM     : chr "0 SOUR @VOID@"
```

Without providing any information you can see that the default xref is
“@VOID@”. This is a special xref value which indicates there is no
record to link to. In this case, all information should be provided in
the object itself, particularly the `@where` property. This is just a
default value - if there is a record, you should put the xref here.

### Multimedia links

Links to Multimedia records are accessed via the `@media_links`
property. Similar to source citations, this can take a character vector
of Multimedia record xrefs, a
[`MediaLink()`](https://jl5000.github.io/gedcomS7/reference/MediaLink.md)
object, or a list of them.

``` r
MediaLink() |> 
  str()
#> <gedcomS7::MediaLink>
#>  @ media_xref: chr "@VOID@"
#>  @ title     : chr(0) 
#>  @ top       : int(0) 
#>  @ left      : int(0) 
#>  @ height    : int(0) 
#>  @ width     : int(0) 
#>  @ GEDCOM    : chr "0 OBJE @VOID@"
```

Again, a @VOID@ xref is given by default and if this is retained, a
`@title` should be provided (any title given will override the title
given in the Multimedia record if one is linked to). The remaining
properties allow you to specify a cropped region of an image.

## Notes

All records (apart from Note records) allow you to attach as many free
text notes as you wish. If a note applies in many places then it is best
to create a Note record which can be referenced everywhere it is needed
with `@note_xrefs`, but otherwise use the `@notes` property.

This property can take notes in a number of ways. The simplest way is
via a character vector. Another way is via a
[`Note()`](https://jl5000.github.io/gedcomS7/reference/Note.md) object,
which also allows you to define some other properties of the note such
as its language and media type.

``` r
indi <- IndividualRecord()

indi@notes <- "This is a single note"

indi@notes <- c("This is a note", "This is an another note")

indi@notes <- Note(text = "This is a single note using a Note object",
                   media_type = "text/plain")
```

Alternatively, you can supply a list which can contain any number of
character or Note elements:

``` r
indi@notes <- list(
  Note(text = "This is one of a number of <b>Note</b> objects. This one is HTML.",
       media_type = "text/html"),
  Note(text = "Esta es una nota",
       language = "es",
       media_type = "text/plain"),
  "This one is a character note"
)
```

You should remember that for any properties that can take multiple
elements, you can append any new values to the existing ones, otherwise
they will be overwritten:

``` r
indi@notes <- append(
  indi@notes,
  list(
    "This is an appended note",
    Note("This is another appended note")
  )
)

indi@notes
#> [[1]]
#> Note:           This is one of a number of <b>Note</b> objects. This 
#>                 one is HTML.
#> 
#> Language:       <Undefined>
#> Format:         text/html
#> Translations:   0
#> Citations:      0
#> 
#> [[2]]
#> Note:           Esta es una nota
#> 
#> Language:       es
#> Format:         text/plain
#> Translations:   0
#> Citations:      0
#> 
#> [[3]]
#> Note:           This one is a character note
#> 
#> Language:       <Undefined>
#> Format:         <Undefined>
#> Translations:   0
#> Citations:      0
#> 
#> [[4]]
#> Note:           This is an appended note
#> 
#> Language:       <Undefined>
#> Format:         <Undefined>
#> Translations:   0
#> Citations:      0
#> 
#> [[5]]
#> Note:           This is another appended note
#> 
#> Language:       <Undefined>
#> Format:         <Undefined>
#> Translations:   0
#> Citations:      0
```

## Creation/modification dates

You have the option of recording when a record is created or changed.
When you push a record to a GEDCOM object, it will record
creation/change dates depending on the values of `@add_creation_dates`
and `@update_change_dates` (these are FALSE by default):

``` r
ged <- new_gedcom()
ged@update_change_dates <- TRUE
ged@add_creation_dates <- TRUE

new_record <- IndividualRecord()
ged <- push_record(ged, new_record)
#> New Individual record added with xref @I1@

# Extract record with creation/change dates added
new_record <- pull_record(ged, "@I1@")
new_record@created
#> Created:       28 FEB 2026
new_record@updated
#> Changed:       28 FEB 2026
```

You can add a time and/or notes to these dates, but that’s probably
overkill.

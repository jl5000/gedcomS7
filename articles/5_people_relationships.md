# Records for defining people and relationships

## Introduction

Information about people and relationships between them are stored in
Individual records and Family records. We will focus on Individual
records first, and then move on to Family records.

## Individuals

``` r
library(gedcomS7)

anakin <- IndividualRecord(
  sex = "M"
)
```

The sex of the individual can be specified with the `sex` property,
which is a single letter: M(ale), F(emale), (Interse)X, or U(nknown). If
no sex is specified, a value of “U” is used.

### Names

A quick and dirty way of adding names is with a character vector
(enclosing forward slashes are GEDCOM’s way of indicating a surname):

``` r
anakin@pers_names <- c("Anakin /Skywalker/", "Darth Vader")
#> Warning in validator(object): Did you forget to enclose the surname in forward
#> slashes?: Darth Vader
```

Individuals can have more than one name. These can be different kinds of
name such as birth name or adoptive name, or if there is uncertainty,
variants of these based on conflicting evidence sources. There is also
the ability to provide translated variants of names. Given this
complexity, a personal name is usually defined using a
[`PersonalName()`](https://jl5000.github.io/gedcomS7/reference/PersonalName.md)
object (or a list of them):

``` r
anakin@pers_names <- list(
  PersonalName(pers_name = "Anakin /Skywalker/",
               name_type = "BIRTH",
               name_pieces = PersonalNamePieces(given = "Anakin",
                                                surname = "Skywalker"),
               notes = "He had this name since the death of Mace Windu."),
  PersonalName(pers_name = "/Darth/ Vader",
               name_type = "AKA",
               type_phrase = "Dark Side name",
               name_translations = PersonalNameTran(pers_name = "Dark Father",
                                                    language = "en"))
)

anakin@pers_names
#> [[1]]
#> Personal Name:   Anakin /Skywalker/
#> Name Type:       BIRTH
#> 
#> Translations:    0
#> Citations:       0
#> Notes:           1
#> 
#> [[2]]
#> Personal Name:   /Darth/ Vader
#> Name Type:       AKA (Dark Side name)
#> 
#> Translations:    1
#> Citations:       0
#> Notes:           0
```

In the example above we have defined two names in a list of
[`PersonalName()`](https://jl5000.github.io/gedcomS7/reference/PersonalName.md)
objects. “Darth” has been enclosed in forward slashes because it is the
closest thing to a family name for Sith Lords!

Name types can also be given (with an optional free text phrase) and
must take one of the values of
[`val_name_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md):

``` r
val_name_types()
#> [1] "AKA"          "BIRTH"        "IMMIGRANT"    "MAIDEN"       "MARRIED"     
#> [6] "PROFESSIONAL" "OTHER"
```

All names should preferably be provided by a set of name pieces,
specifying the given name, surname, nickname, etc.

You can also define a name translation with
[`PersonalNameTran()`](https://jl5000.github.io/gedcomS7/reference/PersonalNameTran.md)
or a list of them. There are also opportunities to record notes and
source citations for each name defined.

Generally, names are arranged in order of importance; the first name is
their primary name (usually birth name).

Once added to the Individual record, the property `@PRIMARY_NAME` will
provide the primary (first) name as a simple character string, and
`@ALL_NAMES` will provide all names as a simple character vector.

``` r
anakin@PRIMARY_NAME
#> [1] "Anakin Skywalker"
anakin@ALL_NAMES
#> [1] "Anakin Skywalker" "Darth Vader"
```

### Facts

Attributes and events associated with an individual, such as birth and
death, are accessed with the `@facts` property. This can be assigned an
[`IndividualEvent()`](https://jl5000.github.io/gedcomS7/reference/IndividualEvent.md)
or
[`IndividualAttribute()`](https://jl5000.github.io/gedcomS7/reference/IndividualAttribute.md)
object (or a list of them). These objects allow you to define a fact,
together with associated date, cause, place/address, and the age of the
individual when the fact applied (among other aspects).

The full list of fact types that can be created are given below:

``` r
val_individual_attribute_types(inc_generic = TRUE)
#>                  Caste   Physical description   Academic achievement 
#>                 "CAST"                 "DSCR"                 "EDUC" 
#>              ID number            Nationality     Number of children 
#>                 "IDNO"                 "NATI"                 "NCHI" 
#>    Number of marriages             Occupation               Property 
#>                  "NMR"                 "OCCU"                 "PROP" 
#>               Religion              Residence Social security number 
#>                 "RELI"                 "RESI"                  "SSN" 
#>         Nobility title        Other attribute 
#>                 "TITL"                 "FACT"
val_individual_event_types(inc_generic = TRUE)
#>          Adoption           Baptism       Bar-mitzvah       Bas-mitzvah 
#>            "ADOP"            "BAPM"            "BARM"            "BASM" 
#>             Birth          Blessing            Burial            Census 
#>            "BIRT"            "BLES"            "BURI"            "CENS" 
#>       Christening Adult christening      Confirmation         Cremation 
#>             "CHR"            "CHRA"            "CONF"            "CREM" 
#>             Death        Emigration   First communion        Graduation 
#>            "DEAT"            "EMIG"            "FCOM"            "GRAD" 
#>       Immigration    Naturalization        Ordination           Probate 
#>            "IMMI"            "NATU"            "ORDN"            "PROB" 
#>        Retirement              Will       Other event 
#>            "RETI"            "WILL"            "EVEN"
```

In the example below, we add 4 facts for Anakin:

``` r
anakin@facts <- list(
  IndividualAttribute(fact_type = "TITL",
                      fact_val = "Lord",
                      notes = "When he was Darth Vader"),
  IndividualAttribute(fact_type = "NMR",
                      fact_val = "1"),
  IndividualEvent(fact_type = "BIRT",
                  place = "Tatooine"),
  IndividualEvent(fact_type = "DEAT",
                  place = "Death Star",
                  cause = "Injuries sustained from the Emperor's lightning")
)
```

If you add a generic event (EVEN) or attribute (FACT), then as well as
defining a fact value, you must also add a fact description which
provides a further classification:

``` r
anakin@facts <- append(
  anakin@facts,
  IndividualEvent(fact_type = "EVEN",
                  fact_val = "Lost legs",
                  fact_desc = "Injury",
                  cause = "Lightsaber duel with Obi-Wan Kenobi",
                  place = "Mustafar")
)
```

You can also assert that an individual never experienced an event via
the `@non_events` property. This takes a
[`NonEvent()`](https://jl5000.github.io/gedcomS7/reference/NonEvent.md)
object or a list of them.

``` r
anakin@non_events <- NonEvent("BARM")
```

A non-event with a date period provided asserts that the event never
happened in that period. If no date period is provided, it asserts the
event never happened at all.

You can see a summary of all facts associated with an individual with
the
[`df_indi_facts()`](https://jl5000.github.io/gedcomS7/reference/df_indi_facts.md)
function.

``` r
skywalkers <- new_gedcom() |> 
  push_record(anakin)
#> New Individual record added with xref @I1@

df_indi_facts(skywalkers, "@I1@")
#>   xref                type       val   desc date      place age
#> 1 @I1@      Nobility title      Lord                           
#> 2 @I1@ Number of marriages         1                           
#> 3 @I1@               Birth                         Tatooine    
#> 4 @I1@               Death                       Death Star    
#> 5 @I1@         Other event Lost legs Injury        Mustafar
```

### Family relationships

One of the most important linkages in an Individual record are with
Family records. An individual can either be linked to a family as a
spouse or as a child.

Below we create a new empty Family record in our GEDCOM object
`skywalkers`, and then link Anakin to this family as a spouse.

``` r
skywalkers <- push_record(skywalkers, FamilyRecord())
#> New Family record added with xref @F1@

anakin <- pull_record(skywalkers, "@I1@")
anakin@fam_links_spou <- FamilyLinkSpouse("@F1@")
```

When we push this record to the GEDCOM object, it will automatically
recognise that a new linkage has appeared, and update the Family record
to reflect this:

``` r
skywalkers <- push_record(skywalkers, anakin)

skywalkers@records@RAW@FAM[["@F1@"]]
#> [1] "0 @F1@ FAM"  "1 HUSB @I1@"
```

Creating linkages between individuals and families in this way can get
quite cumbersome, so a suite of helper functions to define multiple
relationships at once are described in the next section.

## Families

### Members

The key properties of a Family record are for defining the members of
the family (`@husb_xref`, `@wife_xref`, `@chil_xrefs`). These are
usually given as xref links to Individual records. If they do not have
an Individual record then a value of “@VOID@” can be used. However, you
will need to provide this as a named vector describing the person, e.g.

``` r
leia_han <- FamilyRecord()

leia_han@chil_xrefs <- c("Ben Solo" = "@VOID@")
```

A named vector can be used even if an xref to an Individual record is
provided.

The Family records do not need to describe traditional marriage unions -
they can describe non-marital relationships and same-sex relationships.
Children also do not have to be biological. The parameter names are a
legacy from previous versions of the GEDCOM specification, which have
not yet been updated.

### Automation

A suite of helper functions have been created that allow you to add a
spouse, parents, children, or siblings with a single function call.
These create the records and linkages automatically, and you can fill in
the details later. To help you keep track, the functions also allow you
to specify the sexes and/or names of those being added.

As an example, we will add a spouse and two children for Anakin:

``` r
skywalkers <- skywalkers |> 
  add_spouse("@I1@", sex = "F", spou_name = "Padme /Amidala/",
             fam_xref = "@F1@") |> 
  add_children("@F1@", sexes = "MF", chil_names = c("Luke", "Leia"))
#> New Individual record added with xref @I2@
#> New Individual record added with xref @I3@
#> New Individual record added with xref @I4@
```

These functions allow for quite a lot of automation. The
[`add_spouse()`](https://jl5000.github.io/gedcomS7/reference/add_spouse.md)
function automatically creates an Individual record for Padme with her
sex and name, and if there was no existing Family record it would have
created that as well. The
[`add_children()`](https://jl5000.github.io/gedcomS7/reference/add_children.md)
function creates an Individual record for each of the sexes defined.
Names can be optionally provided; if no surnames are included in any
names (indicated by forward slashes) then they will be taken from the
father (or mother if there is no father). The
[`add_parents()`](https://jl5000.github.io/gedcomS7/reference/add_parents.md)
and
[`add_siblings()`](https://jl5000.github.io/gedcomS7/reference/add_siblings.md)
functions work in a similar way.

### Facts

Like Individual records, Family records can also have attributes and
events associated with them, through the `@facts` property. This can be
assigned a
[`FamilyEvent()`](https://jl5000.github.io/gedcomS7/reference/FamilyEvent.md)
or
[`FamilyAttribute()`](https://jl5000.github.io/gedcomS7/reference/FamilyAttribute.md)
object (or a list of them).

The full list of fact types that can be created are given below:

``` r
val_family_attribute_types(inc_generic = TRUE)
#> Number of children          Residence    Other attribute 
#>             "NCHI"             "RESI"             "FACT"
val_family_event_types(inc_generic = TRUE)
#>           Annulment              Census             Divorce       Divorce filed 
#>              "ANUL"              "CENS"               "DIV"              "DIVF" 
#>          Engagement      Marriage banns   Marriage contract    Marriage license 
#>              "ENGA"              "MARB"              "MARC"              "MARL" 
#>            Marriage Marriage settlement         Other event 
#>              "MARR"              "MARS"              "EVEN"
```

Alternatively, the
[`fact_rules_df()`](https://jl5000.github.io/gedcomS7/reference/fact_rules_df.md)
function provides a dataframe of all fact types and the rules
surrounding the population of the `@fact_val` and `@fact_desc`
properties. You can filter this table by providing a search string to
the function:

``` r
fact_rules_df("eve|chr")
#>               fact_name fact_type individual family      fact fact_val_required
#> 9           Christening       CHR       TRUE  FALSE     Event             FALSE
#> 10    Adult christening      CHRA       TRUE  FALSE     Event             FALSE
#> 23          Other event      EVEN       TRUE   TRUE     Event              TRUE
#> 35 Academic achievement      EDUC       TRUE  FALSE Attribute              TRUE
#>    fact_val fact_desc_required
#> 9         Y              FALSE
#> 10        Y              FALSE
#> 23      Any               TRUE
#> 35      Any              FALSE
```

There are some types of fact; census and residence, that can apply for
Individuals and Families. It is highly recommended you do not use these
facts for families, only for individuals. It may intuitively be more
efficient to define an event for an entire family, but other GEDCOM
parsers can have difficulty with this, and defining these facts for each
individual separately is recommended. Despite the name, Family records
are actually more about the union of two people, rather than the family
unit as a whole, and so the remaining events are related to marriages
and divorces.

Like Individual records, events that didn’t happen to a family can be
defined through the `@non_events` property.

## Links to other records

### Individual records

An alternative way of recording the alter ego of Anakin Skywalker would
have been to create a separate Individual record for Darth Vader, and
link them together. This is achievable via the `@alia_xrefs` property.
Whether you include a one-way, or mutual link is up to you.

Individuals and families may also have associations with individuals
that aren’t necessarily familial. You can record these with the
`@associations` property. This property takes an
[`Association()`](https://jl5000.github.io/gedcomS7/reference/Association.md)
object or a list of them.

This object has a void xref pointer by default, as the person may not
have an Individual record, and if this is the case (as in the examples
below), an `@indi_phrase` must be given containing their name. A
`@relation_is` value is mandatory.

``` r
anakin <- pull_record(skywalkers, "@I1@")

anakin@associations <- list(
  Association(indi_phrase = "Obi-Wan Kenobi",
              relation_is = "OTHER",
              relation_phrase = "Master",
              notes = "While he was Anakin Skywalker"),
  Association(indi_phrase = "Emperor Palpatine",
              relation_is = "OTHER",
              relation_phrase = "Master",
              notes = "While he was Darth Vader")
)
```

The `@relation_is` property must take one of the following values. If it
is “OTHER”, then a `@relation_phrase` must be given.

``` r
val_roles()
#>              Child Religious official             Father             Friend 
#>             "CHIL"           "CLERGY"             "FATH"           "FRIEND" 
#>          Godparent            Husband             Mother           Multiple 
#>             "GODP"             "HUSB"             "MOTH"         "MULTIPLE" 
#>           Neighbor         Officiator             Parent             Spouse 
#>            "NGHBR"       "OFFICIATOR"           "PARENT"             "SPOU" 
#>               Wife            Witness              Other 
#>             "WIFE"             "WITN"            "OTHER"
```

### Submitter records

You can also link to Submitter records in the following ways:

- Those that have contributed information about the individual/family
  (`@subm_xrefs`);
- Those that have an interest in the individual’s ancestors
  (`@anci_xrefs`);
- Those that have an interest in the individual’s descendants
  (`@desi_xrefs`).

# gedcomS7 development version

* Bespoke events (`FamilyEvent("EVEN")` and `IndividualEvent("EVEN")`) no longer require a `fact_val` argument
* `add_children()` and `add_siblings` gain a `pedigrees` argument allowing Child to Family links to be defined more specifically
* `Address()` objects no longer treat semi-colons as new lines

# gedcomS7 0.3.0 (24 Oct 2025)

* Added support for dates in the Julian calendar - `DateGregorian()` renamed `DateCalendar()`
* Names and values of `@ext_ids` are now concatenated directly when constructing URIs - trailing slashes must be explicit
* Record and Fact objects no longer have `@GEDCOM_IDENTIFIERS` and `@RESTRICTIONS` properties
* Argument names in `push_record()` made consistent with `pull_record()` 
* Additional validation of arguments in user-facing functions

# gedcomS7 0.2.0 (6 Sep 2025)

* `DateGregorian()` now handles BCE dates with a negative `@year`
* GEDCOM files within zip archives can be imported (GEDzip)
* Minor changes to some internal class names and their inheritance
* Other minor internal improvements

# gedcomS7 0.1.1 (27 Aug 2025)

* No user facing changes
* Major refactoring to introduce set of property creation functions

# gedcomS7 0.1.0 (25 May 2025)

* Initial release

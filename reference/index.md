# Package index

## Classes

### GEDCOM Classes

- [`GedcomHeader`](https://jl5000.github.io/gedcomS7/reference/GedcomHeader.md)
  : Create a GEDCOM header object
- [`GedcomS7()`](https://jl5000.github.io/gedcomS7/reference/GedcomS7.md)
  : Create a GEDCOM object
- [`GedcomSource()`](https://jl5000.github.io/gedcomS7/reference/GedcomSource.md)
  : Create a GEDCOM source object

### Record Classes

- [`FamilyRecord()`](https://jl5000.github.io/gedcomS7/reference/FamilyRecord.md)
  : Create a family record object
- [`IndividualRecord()`](https://jl5000.github.io/gedcomS7/reference/IndividualRecord.md)
  : Create an individual record object
- [`MediaRecord()`](https://jl5000.github.io/gedcomS7/reference/MediaRecord.md)
  : Create a multimedia record object
- [`NoteRecord()`](https://jl5000.github.io/gedcomS7/reference/NoteRecord.md)
  : Create a shared note record object
- [`RepositoryRecord()`](https://jl5000.github.io/gedcomS7/reference/RepositoryRecord.md)
  : Create a repository record object
- [`SourceRecord()`](https://jl5000.github.io/gedcomS7/reference/SourceRecord.md)
  : Create a source record object
- [`SubmitterRecord()`](https://jl5000.github.io/gedcomS7/reference/SubmitterRecord.md)
  : Create a submitter record object

### Location Classes

- [`Place()`](https://jl5000.github.io/gedcomS7/reference/Place.md) :
  Create a place structure object
- [`Address()`](https://jl5000.github.io/gedcomS7/reference/Address.md)
  : Create an address object

### Date/Time Classes

- [`DateApprox()`](https://jl5000.github.io/gedcomS7/reference/DateApprox.md)
  : Create a GEDCOM Approximate Date object
- [`DateCalendar()`](https://jl5000.github.io/gedcomS7/reference/DateCalendar.md)
  : Create a GEDCOM Calendar Date object
- [`DateExact()`](https://jl5000.github.io/gedcomS7/reference/DateExact.md)
  : Create a GEDCOM Exact Date object
- [`DatePeriod()`](https://jl5000.github.io/gedcomS7/reference/DatePeriod.md)
  : Create a GEDCOM Date Period object
- [`DateRange()`](https://jl5000.github.io/gedcomS7/reference/DateRange.md)
  : Create a GEDCOM Date Range object
- [`DateSorting()`](https://jl5000.github.io/gedcomS7/reference/DateSorting.md)
  : Create a GEDCOM Sorting Date object
- [`DateValue()`](https://jl5000.github.io/gedcomS7/reference/DateValue.md)
  : Create a GEDCOM Date Value object
- [`Time()`](https://jl5000.github.io/gedcomS7/reference/Time.md) :
  Create a time object
- [`ChangeDate`](https://jl5000.github.io/gedcomS7/reference/ChangeDate.md)
  : Create a change date object
- [`CreationDate`](https://jl5000.github.io/gedcomS7/reference/CreationDate.md)
  : Create a creation date object
- [`date_exact_current()`](https://jl5000.github.io/gedcomS7/reference/date_exact_current.md)
  : Create a GEDCOM Exact Date object for today

### Record Linkage Classes

- [`FamilyLinkChild()`](https://jl5000.github.io/gedcomS7/reference/FamilyLinkChild.md)
  : Create a family link (as child) object
- [`FamilyLinkSpouse()`](https://jl5000.github.io/gedcomS7/reference/FamilyLinkSpouse.md)
  : Create a family link (as spouse) object
- [`MediaLink()`](https://jl5000.github.io/gedcomS7/reference/MediaLink.md)
  : Create a multimedia link object
- [`RepositoryCitation()`](https://jl5000.github.io/gedcomS7/reference/RepositoryCitation.md)
  : Create a repository citation object
- [`SourceCitation()`](https://jl5000.github.io/gedcomS7/reference/SourceCitation.md)
  : Create a source citation object
- [`Association()`](https://jl5000.github.io/gedcomS7/reference/Association.md)
  : Create an association object

### Individual/Family Fact Classes

- [`FamilyEvent()`](https://jl5000.github.io/gedcomS7/reference/FamilyEvent.md)
  : Create a family event object
- [`IndividualEvent()`](https://jl5000.github.io/gedcomS7/reference/IndividualEvent.md)
  : Create an individual event object
- [`NonEvent()`](https://jl5000.github.io/gedcomS7/reference/NonEvent.md)
  : Create a non-event object
- [`FamilyAttribute()`](https://jl5000.github.io/gedcomS7/reference/FamilyAttribute.md)
  : Create a family attribute object
- [`IndividualAttribute()`](https://jl5000.github.io/gedcomS7/reference/IndividualAttribute.md)
  : Create an individual attribute object
- [`fact_rules_df()`](https://jl5000.github.io/gedcomS7/reference/fact_rules_df.md)
  : Property requirements for fact objects.

### Other Classes

- [`Note()`](https://jl5000.github.io/gedcomS7/reference/Note.md) :
  Create a note structure object
- [`PersonalName()`](https://jl5000.github.io/gedcomS7/reference/PersonalName.md)
  : Create a personal name object
- [`PersonalNamePieces()`](https://jl5000.github.io/gedcomS7/reference/PersonalNamePieces.md)
  : Create a name pieces object
- [`PersonalNameTran()`](https://jl5000.github.io/gedcomS7/reference/PersonalNameTran.md)
  : Create a name translation object
- [`Ordinance()`](https://jl5000.github.io/gedcomS7/reference/Ordinance.md)
  : Create an individual ordinance object
- [`SpouseSealing()`](https://jl5000.github.io/gedcomS7/reference/SpouseSealing.md)
  : Create a spouse sealing object
- [`TranslationText()`](https://jl5000.github.io/gedcomS7/reference/TranslationText.md)
  : Create a text translation object
- [`FactsRecorded()`](https://jl5000.github.io/gedcomS7/reference/FactsRecorded.md)
  : Create an object recording facts covered in a source record
- [`SourceCallNumber()`](https://jl5000.github.io/gedcomS7/reference/SourceCallNumber.md)
  : Create a source call number object
- [`MediaFile()`](https://jl5000.github.io/gedcomS7/reference/MediaFile.md)
  : Create a media file object

## GEDCOM Creation

Functions for creating, importing, and exporting pre-populated GEDCOM
files

- [`new_gedcom()`](https://jl5000.github.io/gedcomS7/reference/new_gedcom.md)
  : Create a new GEDCOM object
- [`read_gedcom()`](https://jl5000.github.io/gedcomS7/reference/read_gedcom.md)
  : Import a GEDCOM file
- [`write_gedcom()`](https://jl5000.github.io/gedcomS7/reference/write_gedcom.md)
  : Save a gedcom object to disk as a GEDCOM file

## Record Editing

Functions to edit GEDCOM records

- [`pull_record()`](https://jl5000.github.io/gedcomS7/reference/pull_record.md)
  : Pull a record from a GEDCOM object for editing
- [`push_record()`](https://jl5000.github.io/gedcomS7/reference/push_record.md)
  : Push an edited record back into a GEDCOM object

## Batch Creation/Deletion of Records

Functions that allow you to create records for relatives and delete
multiple records at once

- [`add_children()`](https://jl5000.github.io/gedcomS7/reference/add_children.md)
  : Create children records for a family
- [`add_parents()`](https://jl5000.github.io/gedcomS7/reference/add_parents.md)
  : Add parent records for an individual
- [`add_siblings()`](https://jl5000.github.io/gedcomS7/reference/add_siblings.md)
  : Create sibling records for an individual
- [`add_spouse()`](https://jl5000.github.io/gedcomS7/reference/add_spouse.md)
  : Add a spouse record for an individual
- [`rm_living()`](https://jl5000.github.io/gedcomS7/reference/rm_living.md)
  : Remove living individuals in a GEDCOM object
- [`rm_records()`](https://jl5000.github.io/gedcomS7/reference/rm_records.md)
  : Remove records from a GEDCOM object

## Finding Records

Functions that allow you to find xrefs of records that match criteria

- [`get_ancestors()`](https://jl5000.github.io/gedcomS7/reference/get_ancestors.md)
  : Identify all ancestors for an individual
- [`get_descendants()`](https://jl5000.github.io/gedcomS7/reference/get_descendants.md)
  : Identify all descendants for an individual
- [`get_fam_as_child()`](https://jl5000.github.io/gedcomS7/reference/get_fam_as_child.md)
  : Identify all families for an individual where they are a child
- [`get_fam_as_spouse()`](https://jl5000.github.io/gedcomS7/reference/get_fam_as_spouse.md)
  : Identify all families for an individual where they are a partner
- [`get_fam_children()`](https://jl5000.github.io/gedcomS7/reference/get_fam_children.md)
  : Identify all children in a family
- [`get_fam_partners()`](https://jl5000.github.io/gedcomS7/reference/get_fam_partners.md)
  : Identify all partners in a family
- [`get_indi_children()`](https://jl5000.github.io/gedcomS7/reference/get_indi_children.md)
  : Identify all children for an individual
- [`get_indi_cousins()`](https://jl5000.github.io/gedcomS7/reference/get_indi_cousins.md)
  : Identify all cousins for an individual
- [`get_indi_fathers()`](https://jl5000.github.io/gedcomS7/reference/get_indi_fathers.md)
  : Identify all fathers for an individual
- [`get_indi_mothers()`](https://jl5000.github.io/gedcomS7/reference/get_indi_mothers.md)
  : Identify all mothers for an individual
- [`get_indi_parents()`](https://jl5000.github.io/gedcomS7/reference/get_indi_parents.md)
  : Identify all parents for an individual
- [`get_indi_partners()`](https://jl5000.github.io/gedcomS7/reference/get_indi_partners.md)
  : Identify all partners for an individual
- [`get_indi_siblings()`](https://jl5000.github.io/gedcomS7/reference/get_indi_siblings.md)
  : Identify all siblings for an individual
- [`get_records_by_pattern()`](https://jl5000.github.io/gedcomS7/reference/get_records_by_pattern.md)
  : Identify all records that contain a pattern
- [`get_supporting_recs()`](https://jl5000.github.io/gedcomS7/reference/get_supporting_recs.md)
  : Identify all supporting records for a set of records
- [`get_unused_recs()`](https://jl5000.github.io/gedcomS7/reference/get_unused_recs.md)
  : Identify unreferenced records

## Summarising Records

Functions that allow you to generate dataframe summaries of record types
and facts

- [`df_fam()`](https://jl5000.github.io/gedcomS7/reference/df_fam.md) :
  Summarise Family records in a dataframe
- [`df_fam_facts()`](https://jl5000.github.io/gedcomS7/reference/df_fam_facts.md)
  : Summarise a family's attributes/events in a dataframe
- [`df_indi()`](https://jl5000.github.io/gedcomS7/reference/df_indi.md)
  : Summarise Individual records in a dataframe
- [`df_indi_facts()`](https://jl5000.github.io/gedcomS7/reference/df_indi_facts.md)
  : Summarise an individual's attributes/events in a dataframe
- [`df_media()`](https://jl5000.github.io/gedcomS7/reference/df_media.md)
  : Summarise Multimedia records in a dataframe
- [`df_note()`](https://jl5000.github.io/gedcomS7/reference/df_note.md)
  : Summarise Note records in a dataframe
- [`df_repo()`](https://jl5000.github.io/gedcomS7/reference/df_repo.md)
  : Summarise Repository records in a dataframe
- [`df_sour()`](https://jl5000.github.io/gedcomS7/reference/df_sour.md)
  : Summarise Source records in a dataframe
- [`df_subm()`](https://jl5000.github.io/gedcomS7/reference/df_subm.md)
  : Summarise Submitter records in a dataframe

## Parsing Functions

Functions for parsing GEDCOM dates/ages into R dates/numerics

- [`parse_gedcom_age()`](https://jl5000.github.io/gedcomS7/reference/parse_gedcom_age.md)
  : Convert a GEDCOM age at event into decimalised years
- [`parse_gedcom_date()`](https://jl5000.github.io/gedcomS7/reference/parse_gedcom_date.md)
  : Convert a GEDCOM date into a date object

## Controlled Values

Functions to return vectors of controlled values

- [`val_record_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md)
  [`val_adoptive_parents()`](https://jl5000.github.io/gedcomS7/reference/lookups.md)
  [`val_individual_attribute_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md)
  [`val_individual_event_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md)
  [`val_family_event_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md)
  [`val_family_attribute_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md)
  [`val_event_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md)
  [`val_attribute_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md)
  [`val_fact_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md)
  [`val_individual_ordinance_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md)
  [`val_family_ordinance_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md)
  [`val_ordinance_states()`](https://jl5000.github.io/gedcomS7/reference/lookups.md)
  [`val_medium_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md)
  [`val_pedigree_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md)
  [`val_certainty()`](https://jl5000.github.io/gedcomS7/reference/lookups.md)
  [`val_restriction()`](https://jl5000.github.io/gedcomS7/reference/lookups.md)
  [`val_roles()`](https://jl5000.github.io/gedcomS7/reference/lookups.md)
  [`val_sexes()`](https://jl5000.github.io/gedcomS7/reference/lookups.md)
  [`val_confidence_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md)
  [`val_name_types()`](https://jl5000.github.io/gedcomS7/reference/lookups.md)
  : Lookup values

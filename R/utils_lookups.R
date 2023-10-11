
#' Lookup values
#' @return A vector of allowed values.
#' @export
val_record_types <- function(){
  c(Individual = "indi", Family = "fam", Source = "sour", Submitter = "subm",
    Repository = "repo", Multimedia = "media", Note = "note")
}

#' @rdname val_record_types
#' @export
val_adoptive_parents <- function() {
  c(Husband = "HUSB", Wife = "WIFE", Both = "BOTH")
}

#' @rdname val_record_types
#' @param inc_generic Whether to include a generic attribute.
#' @export
val_individual_attribute_types <- function(inc_generic = FALSE) {
  vals <- c(
    Caste = "CAST", 
    `Physical description` = "DSCR", 
    `Academic achievement` = "EDUC", 
    `ID number` = "IDNO",
    Nationality = "NATI", 
    `Number of children` = "NCHI", 
    `Number of marriages` = "NMR", 
    Occupation = "OCCU",
    Property = "PROP", 
    Religion = "RELI", 
    Residence = "RESI",
    `Social security number` = "SSN",
    `Nobility title` = "TITL"
  )
  if(!inc_generic) return(vals)
  c(vals, `Other individual attribute` = "FACT")
}

#' @rdname val_record_types
#' @param inc_generic Whether to include a generic event.
#' @export
val_individual_event_types <- function(inc_generic = FALSE) {
  vals <- c(
    Adoption = "ADOP", 
    Baptism = "BAPM",
    `Bar-mitzvah` = "BARM", 
    `Bas-mitzvah` = "BASM",
    Birth = "BIRT", 
    Blessing = "BLES",
    Burial = "BURI",
    Census = "CENS",
    Christening = "CHR", 
    `Adult christening` = "CHRA", 
    Confirmation = "CONF", 
    Cremation = "CREM",
    Death = "DEAT", 
    Emigration = "EMIG", 
    `First communion` = "FCOM", 
    Graduation = "GRAD", 
    Immigration = "IMMI", 
    Naturalization = "NATU",
    Ordination = "ORDN",
    Probate = "PROB", 
    Retirement = "RETI",
    Will = "WILL"
  )
  if(!inc_generic) return(vals)
  c(vals, `Other individual event` = "EVEN")
}

#' @rdname val_individual_event_types
#' @export
val_family_event_types <- function(inc_generic = FALSE) {
  vals <- c(
    Annulment = "ANUL", 
    Census = "CENS", 
    Divorce = "DIV", 
    `Divorce filed` = "DIVF",
    Engagement = "ENGA", 
    `Marriage banns` = "MARB", 
    `Marriage contract` = "MARC", 
    `Marriage license` = "MARL", 
    Marriage = "MARR",
    `Marriage settlement` = "MARS"
  )
  if(!inc_generic) return(vals)
  c(vals, `Other family event` = "EVEN")
}


#' @rdname val_individual_attribute_types
#' @export
val_family_attribute_types <- function(inc_generic = FALSE) {
  vals <- c(
    `Number of children` = "NCHI",
    Residence = "RESI"
  )
  if(!inc_generic) return(vals)
  c(vals, `Other family attribute` = "FACT")
}

#' @rdname val_individual_event_types
#' @export
val_event_types <- function(inc_generic = FALSE) {
  c(
    val_individual_event_types(inc_generic),
    val_family_event_types(inc_generic)
  )
}

#' @rdname val_individual_attribute_types
#' @export
val_attribute_types <- function(inc_generic = FALSE) {
  c(
    val_individual_attribute_types(inc_generic),
    val_family_attribute_types(inc_generic)
  )
}

#' @rdname val_record_types
#' @param inc_generic Whether to include a generic fact.
#' @export
val_fact_types <- function(inc_generic = FALSE) {
  c(
    val_event_types(inc_generic),
    val_individual_attribute_types(inc_generic),
    val_family_attribute_types(inc_generic)
  )
}

#' @rdname val_record_types
#' @export
val_individual_ordinance_types <- function(){
  c(
    Baptism = "BAPL", 
    Confirmation = "CONL", 
    Endowment = "ENDL", 
    Initiatory = "INIL",
    `Child sealing` = "SLGC"
  )
}

#' @rdname val_record_types
#' @export
val_family_ordinance_types <- function(){
  c(
    `Spouse sealing` = "SLGS"
  )
}

#' @rdname val_record_types
#' @param ord_type One of the values in `val_individual_ordinance_types()` or
#' `val_family_ordinance_types()`.
#' @export
val_ordinance_states <- function(ord_type){
  types <- c(
    "COMPLETED","EXCLUDED","PRE_1970","STILLBORN","SUBMITTED","UNCLEARED"
  )
  if(ord_type == "SLGC") types <- c(types, "BIC")
  if(ord_type == "SLGS") types <- c(types, "CANCELED")
  if(ord_type != "SLGC") types <- c(types, "CHILD")
  if(ord_type != "SLGC") types <- c(types, "INFANT")
  if(ord_type %in% c("SLGC","SLGS")) types <- c(types, "DNS")
  if(ord_type == "SLGS") types <- c(types, "DNS_CAN")

  types
}

#' @rdname val_record_types
#' @export
val_medium_types <- function() {
  vals <- c("AUDIO","BOOK","CARD","ELECTRONIC","FICHE","FILM",
            "MAGAZINE","MANUSCRIPT","MAP","NEWSPAPER","PHOTO",
            "TOMBSTONE","VIDEO","OTHER")
  stats::setNames(vals, tools::toTitleCase(vals))
}

#' @rdname val_record_types
#' @export
val_pedigree_types <- function() {
  vals <- c("BIRTH", "ADOPTED", "FOSTER", "SEALING", "OTHER")
  stats::setNames(vals, tools::toTitleCase(vals))
}

#' @rdname val_record_types
#' @export
val_certainty <- function() {
  c(
    Unreliable = "0",
    Questionable = "1",
    Secondary = "2",
    Primary = "3"
  )
}

#' @rdname val_record_types
#' @export
val_restriction <- function(){
  vals <- c("CONFIDENTIAL", "LOCKED", "PRIVACY")
  stats::setNames(vals, tools::toTitleCase(vals))
}

#' @rdname val_record_types
#' @export
val_roles <- function() {
  c(
    Child = "CHIL",
    `Religious official` = "CLERGY",
    Father = "FATH",
    Friend = "FRIEND",
    Godparent = "GODP",
    Husband = "HUSB", 
    Mother = "MOTH",
    Multiple = "MULTIPLE",
    Neighbor = "NGHBR",
    Officiator = "OFFICIATOR",
    Parent = "PARENT",
    Spouse = "SPOU",
    Wife = "WIFE", 
    Witness = "WITN",
    Other = "OTHER"
  )
}

#' @rdname val_record_types
#' @export
val_sexes <- function() {
  c(Unknown = "U", Male = "M", Female = "F", `Non-binary` = "X")
}

#' @rdname val_record_types
#' @export
val_confidence_types <- function() {
  vals <- c("CHALLENGED", "DISPROVEN", "PROVEN")
  stats::setNames(vals, tools::toTitleCase(vals))
}

#' @rdname val_record_types
#' @export
val_name_types <- function() {
  c(
    "AKA","BIRTH","IMMIGRANT","MAIDEN","MARRIED","PROFESSIONAL","OTHER"
  )
}


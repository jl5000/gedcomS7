
#' Lookup values
#' @rdname lookups
#' @param inc_generic Whether to include a generic facts.
#' @param ord_type One of the values in `val_individual_ordinance_types()` or
#' `val_family_ordinance_types()`.
#' @returns A vector of allowed values.
#' @export
val_record_types <- function(){
  c(Individual = "indi", Family = "fam", Source = "sour", Submitter = "subm",
    Repository = "repo", Multimedia = "media", Note = "note")
}

#' @rdname lookups
#' @export
val_adoptive_parents <- function() {
  c(Husband = "HUSB", Wife = "WIFE", Both = "BOTH")
}

#' @rdname lookups
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
  c(vals, `Other attribute` = "FACT")
}

#' @rdname lookups
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
  c(vals, `Other event` = "EVEN")
}

#' @rdname lookups
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
  c(vals, `Other event` = "EVEN")
}


#' @rdname lookups
#' @export
val_family_attribute_types <- function(inc_generic = FALSE) {
  vals <- c(
    `Number of children` = "NCHI",
    Residence = "RESI"
  )
  if(!inc_generic) return(vals)
  c(vals, `Other attribute` = "FACT")
}

#' @rdname lookups
#' @export
val_event_types <- function(inc_generic = FALSE) {
  c(
    val_individual_event_types(inc_generic),
    val_family_event_types(inc_generic)
  )
}

#' @rdname lookups
#' @export
val_attribute_types <- function(inc_generic = FALSE) {
  c(
    val_individual_attribute_types(inc_generic),
    val_family_attribute_types(inc_generic)
  )
}

#' @rdname lookups
#' @export
val_fact_types <- function(inc_generic = FALSE) {
  fcts <- c(
    val_event_types(inc_generic),
    val_individual_attribute_types(inc_generic),
    val_family_attribute_types(inc_generic)
  )
  fct_nms <- names(fcts)
  fcts <- unique(fcts)
  names(fcts) <- unique(fct_nms)
  fcts
}

#' @rdname lookups
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

#' @rdname lookups
#' @export
val_family_ordinance_types <- function(){
  c(
    `Spouse sealing` = "SLGS"
  )
}

#' @rdname lookups
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

#' @rdname lookups
#' @export
val_medium_types <- function() {
  vals <- c("AUDIO","BOOK","CARD","ELECTRONIC","FICHE","FILM",
            "MAGAZINE","MANUSCRIPT","MAP","NEWSPAPER","PHOTO",
            "TOMBSTONE","VIDEO","OTHER")
  stats::setNames(vals, tools::toTitleCase(vals))
}

#' @rdname lookups
#' @export
val_pedigree_types <- function() {
  vals <- c("BIRTH", "ADOPTED", "FOSTER", "SEALING", "OTHER")
  stats::setNames(vals, tools::toTitleCase(vals))
}

#' @rdname lookups
#' @export
val_certainty <- function() {
  c(
    Unreliable = "0",
    Questionable = "1",
    Secondary = "2",
    Primary = "3"
  )
}

#' @rdname lookups
#' @export
val_restriction <- function(){
  vals <- c("CONFIDENTIAL", "LOCKED", "PRIVACY")
  stats::setNames(vals, tools::toTitleCase(vals))
}

#' @rdname lookups
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

#' @rdname lookups
#' @export
val_sexes <- function() {
  c(Unknown = "U", Male = "M", Female = "F", `Non-binary` = "X")
}

#' @rdname lookups
#' @export
val_confidence_types <- function() {
  vals <- c("CHALLENGED", "DISPROVEN", "PROVEN")
  stats::setNames(vals, tools::toTitleCase(vals))
}

#' @rdname lookups
#' @export
val_name_types <- function() {
  c(
    "AKA","BIRTH","IMMIGRANT","MAIDEN","MARRIED","PROFESSIONAL","OTHER"
  )
}


val_facts_rules <- function(){

  all_df <- data.frame(fact_name = names(val_fact_types(TRUE)),
                       fact_type = unname(val_fact_types(TRUE)))
  
  # General rules
  all_df$individual <- all_df$fact_type %in% c(val_individual_attribute_types(TRUE),
                                              val_individual_event_types(TRUE))
  all_df$family <- all_df$fact_type %in% c(val_family_attribute_types(TRUE),
                                          val_family_event_types(TRUE))
  all_df$fact <- ifelse(all_df$fact_type %in% c(val_family_event_types(TRUE),
                                               val_individual_event_types(TRUE)),
                       "Event", "Attribute")
  all_df$fact_val_required <- all_df$fact == "Attribute"
  all_df$fact_val <- ifelse(all_df$fact == "Event", "Y", "Any")
  
  # Exceptions
  all_df$fact_val <- ifelse(all_df$fact_type %in% c("NCHI","NMR"),
                            "Number", all_df$fact_val)
  all_df$fact_val_required <- ifelse(all_df$fact_type %in% c("FACT","EVEN"),
                                     TRUE, all_df$fact_val_required)
  all_df$fact_val_required <- ifelse(all_df$fact_type %in% c("RESI"),
                                     FALSE, all_df$fact_val_required)
  all_df$fact_val <- ifelse(all_df$fact_type %in% c("FACT","EVEN"),
                            "Any", all_df$fact_val)
  all_df$fact_desc_required <- all_df$fact_type %in% c("FACT", "EVEN", "IDNO")
  
  all_df
}
  
  
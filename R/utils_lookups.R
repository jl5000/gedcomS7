
val_record_types <- function(){
  c(Individual = "indi", `Family Group` = "famg", Source = "sour",
    Repository = "repo", Multimedia = "media", Note = "note")
}

#' Get valid values for inputs
#' 
#' These functions return the valid values that are allowed by the GEDCOM Specification.
#'
#' @return A character vector of valid values.
#' @export
val_languages <- function() {
  c("English", "Afrikaans", "Albanian", "Anglo-Saxon", "Catalan", "Catalan_Spn", "Czech", 
    "Danish", "Dutch", "Esperanto", "Estonian", "Faroese", "Finnish", 
    "French", "German", "Hawaiian", "Hungarian", "Icelandic", "Indonesian", 
    "Italian", "Latvian", "Lithuanian", "Navaho", "Norwegian", "Polish", 
    "Portuguese", "Romanian", "Serbo_Croa", "Slovak", "Slovene", "Spanish", 
    "Swedish", "Turkish", "Wendic")
}

#' @rdname val_languages
#' @export
val_sexes <- function() {
  c(Unknown = "U", Male = "M", Female = "F", `Non-binary` = "X")
}

#' @rdname val_languages
#' @export
val_multimedia_formats <- function() {
  c("AAC", "AVI", "BMP", "ePub", "FLAC", "GIF", "JPEG", "MKV",
    "mobi", "MP3", "PCX", "PDF", "PNG", "TIFF", "WAV")
}

#' @rdname val_languages
#' @export
val_pedigree_linkage_types <- function() {
  vals <- c("BIRTH", "ADOPTED", "FOSTER", "OTHER")
  stats::setNames(vals, tools::toTitleCase(vals))
}

#' @rdname val_languages
#' @export
val_source_media_types <- function() {
  vals <- c("AUDIO","BOOK","CARD","ELECTRONIC","FICHE","FILM",
            "MAGAZINE","MANUSCRIPT","MAP","NEWSPAPER","PHOTO",
            "TOMBSTONE","VIDEO","OTHER")
  stats::setNames(vals, tools::toTitleCase(vals))
}

#' @rdname val_languages
#' @export
val_adoptive_parents <- function() {
  c(Husband = "HUSB", Wife = "WIFE", Both = "BOTH")
}

#' @rdname val_languages
#' @export
val_restriction <- function(){
  vals <- c("CONFIDENTIAL", "LOCKED", "PRIVACY")
  stats::setNames(vals, tools::toTitleCase(vals))
}

#' @rdname val_languages
#' @export
val_roles <- function() {
  c(Child = "CHIL",
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
    Other = "OTHER")
}

#' @rdname val_languages
#' @export
val_attribute_types <- function() {
  c(Caste = "CAST", 
    `Academic achievement` = "EDUC", 
    `National ID number` = "IDNO",
    Nationality = "NATI", 
    `Nobility title` = "TITL", 
    `Number of children` = "NCHI", 
    `Number of relationships` = "NMR", 
    Occupation = "OCCU",
    `Physical description` = "DSCR", 
    Property = "PROP", 
    Religion = "RELI", 
    Residence = "RESI",
    `Other individual attribute` = "FACT")
}

#' @rdname val_languages
#' @export
val_family_event_types <- function() {
  c(Annulment = "ANUL", 
    Census = "CENS", 
    Divorce = "DIV", 
    `Divorce filed` = "DIVF",
    Engagement = "ENGA", 
    `Marriage banns` = "MARB", 
    `Marriage contract` = "MARC", 
    `Marriage license` = "MARL", 
    `Marriage settlement` = "MARS",
    Relationship = "MARR",
    Residence = "RESI",
    `Other event` = "EVEN")
}

#' @rdname val_languages
#' @export
val_individual_event_types <- function() {
  c(Adoption = "ADOP", 
    `Adult christening` = "CHRA", 
    Baptism = "BAPM",
    `Bar-mitzvah` = "BARM", 
    `Bas-mitzvah` = "BASM",
    Birth = "BIRT", 
    Burial = "BURI",
    Census = "CENS",
    Christening = "CHR", 
    Confirmation = "CONF", 
    Cremation = "CREM",
    Death = "DEAT", 
    Emigration = "EMIG", 
    `First communion` = "FCOM", 
    Graduation = "GRAD", 
    Immigration = "IMMI", 
    Naturalization = "NATU",
    Probate = "PROB", 
    Retirement = "RETI",
    Will = "WILL",
    `Other event` = "EVEN")
}

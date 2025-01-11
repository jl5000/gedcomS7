
get_records <- function(x, xrefs, rec_type){
  rec_list <- S7::prop(x@records@RAW, rec_type)
  xrefs <- xrefs %||% names(rec_list)
  invalid <- setdiff(xrefs, names(rec_list))
  if(length(invalid) > 0){
    xrefs <- setdiff(xrefs, invalid)
    warning("The following xrefs are not of the right type: ", toString(invalid))
  }
  rec_list <- rec_list[xrefs]
}

#' Summarise records of a particular type in a dataframe
#'
#' @param x A gedcom object.
#' @param xrefs A vector of xrefs to summarise. If this is left NULL,
#' all relevant xrefs will be used.
#'
#' @returns A dataframe summarising a record on each row.
#' @export
df_indi <- function(x, xrefs = NULL){
  rec_list <- get_records(x, xrefs, "INDI")
  if(length(rec_list) == 0) return(NULL)
  
  df <- data.frame(
    xref = names(rec_list),
    name = vapply(rec_list, \(lines) chronify(find_ged_values(lines, "NAME")), FUN.VALUE = character(1)),
    sex = vapply(rec_list, \(lines) chronify(find_ged_values(lines, "SEX")), FUN.VALUE = character(1)),
    birth_date = vapply(rec_list, \(lines) chronify(find_ged_values(lines, c("BIRT","DATE"))), FUN.VALUE = character(1)),
    birth_place = vapply(rec_list, \(lines) chronify(find_ged_values(lines, c("BIRT","PLAC"))), FUN.VALUE = character(1)),
    is_alive = vapply(rec_list, \(lines) is_alive(lines), FUN.VALUE = logical(1)),
    death_date = vapply(rec_list, \(lines) chronify(find_ged_values(lines, c("DEAT","DATE"))), FUN.VALUE = character(1)),
    death_place = vapply(rec_list, \(lines) chronify(find_ged_values(lines, c("DEAT","PLAC"))), FUN.VALUE = character(1)),
    fam_as_child = vapply(names(rec_list), \(xref) get_fam_as_child(x, xref, "BIRTH") |> 
                            paste(collapse = ";"), FUN.VALUE = character(1)),
    fam_as_spouse = vapply(names(rec_list), \(xref) get_fam_as_spouse(x, xref) |> 
                             paste(collapse = ";"), FUN.VALUE = character(1)),
    last_modified = vapply(rec_list, \(lines) chronify(find_ged_values(lines, c("CHAN","DATE"))), FUN.VALUE = character(1))
  )
  
  rownames(df) <- NULL
  df
}

#' @rdname df_indi
#' @export
df_fam <- function(x, xrefs = NULL){
  rec_list <- get_records(x, xrefs, "FAM")
  if(length(rec_list) == 0) return(NULL)
  
  df <- data.frame(
    xref = names(rec_list),
    husb_xref = vapply(rec_list, \(lines) chronify(find_ged_values(lines, "HUSB")), FUN.VALUE = character(1)),
    wife_xref = vapply(rec_list, \(lines) chronify(find_ged_values(lines, "WIFE")), FUN.VALUE = character(1)),
    chil_xref = vapply(rec_list, \(lines) find_ged_values(lines, "CHIL") |>
                          paste(collapse = ";"), FUN.VALUE = character(1)),
    marr_date = vapply(rec_list, \(lines) chronify(find_ged_values(lines, c("MARR","DATE"))), FUN.VALUE = character(1)),
    marr_place = vapply(rec_list, \(lines) chronify(find_ged_values(lines, c("MARR","PLAC"))), FUN.VALUE = character(1)),
    last_modified = vapply(rec_list, \(lines) chronify(find_ged_values(lines, c("CHAN","DATE"))), FUN.VALUE = character(1))
  )
  
  rownames(df) <- NULL
  df
}

#' @rdname df_indi
#' @export
df_sour <- function(x, xrefs = NULL){
  rec_list <- get_records(x, xrefs, "SOUR")
  if(length(rec_list) == 0) return(NULL)
  
  df <- data.frame(
    xref = names(rec_list),
    originator = vapply(rec_list, \(lines) chronify(find_ged_values(lines, "AUTH")), FUN.VALUE = character(1)),
    title = vapply(rec_list, \(lines) chronify(find_ged_values(lines, "TITL")), FUN.VALUE = character(1)),
    repo_xref = vapply(rec_list, \(lines) find_ged_values(lines, "REPO") |>
                         paste(collapse = ";"), FUN.VALUE = character(1)),
    last_modified = vapply(rec_list, \(lines) chronify(find_ged_values(lines, c("CHAN","DATE"))), FUN.VALUE = character(1))
  )
  
  rownames(df) <- NULL
  df
}

#' @rdname df_indi
#' @export
df_repo <- function(x, xrefs = NULL){
  rec_list <- get_records(x, xrefs, "REPO")
  if(length(rec_list) == 0) return(NULL)
  
  df <- data.frame(
    xref = names(rec_list),
    name = vapply(rec_list, \(lines) chronify(find_ged_values(lines, "NAME")), FUN.VALUE = character(1)),
    address = vapply(rec_list, \(lines) chronify(find_ged_values(lines, "ADDR")), FUN.VALUE = character(1)),
    last_modified = vapply(rec_list, \(lines) chronify(find_ged_values(lines, c("CHAN","DATE"))), FUN.VALUE = character(1))
  )
  
  rownames(df) <- NULL
  df
}

#' @rdname df_indi
#' @export
df_media <- function(x, xrefs = NULL){
  rec_list <- get_records(x, xrefs, "OBJE")
  if(length(rec_list) == 0) return(NULL)
  
  df <- data.frame(
    xref = names(rec_list),
    num_files = vapply(rec_list, \(lines) length(find_ged_values(lines, "FILE")), FUN.VALUE = integer(1)),
    paths = vapply(rec_list, \(lines) find_ged_values(lines, "FILE") |>
                     paste(collapse = ";"), FUN.VALUE = character(1)),
    last_modified = vapply(rec_list, \(lines) chronify(find_ged_values(lines, c("CHAN","DATE"))), FUN.VALUE = character(1))
  )
  
  rownames(df) <- NULL
  df
}

#' @rdname df_indi
#' @export
df_note <- function(x, xrefs = NULL){
  rec_list <- get_records(x, xrefs, "SNOTE")
  if(length(rec_list) == 0) return(NULL)
  
  df <- data.frame(
    xref = names(rec_list),
    language = vapply(rec_list, \(lines) chronify(find_ged_values(lines, "LANG")), FUN.VALUE = character(1)),
    last_modified = vapply(rec_list, \(lines) chronify(find_ged_values(lines, c("CHAN","DATE"))), FUN.VALUE = character(1))
  )
  
  rownames(df) <- NULL
  df
}

#' @rdname df_indi
#' @export
df_subm <- function(x, xrefs = NULL){
  rec_list <- get_records(x, xrefs, "SUBM")
  if(length(rec_list) == 0) return(NULL)
  
  df <- data.frame(
    xref = names(rec_list),
    name = vapply(rec_list, \(lines) chronify(find_ged_values(lines, "NAME")), FUN.VALUE = character(1)),
    address = vapply(rec_list, \(lines) chronify(find_ged_values(lines, "ADDR")), FUN.VALUE = character(1)),
    last_modified = vapply(rec_list, \(lines) chronify(find_ged_values(lines, c("CHAN","DATE"))), FUN.VALUE = character(1))
  )
  
  rownames(df) <- NULL
  df
}

#' Summarise an individual's attributes/events in a dataframe
#'
#' @param x A gedcom object.
#' @param xref The cross-reference identifier of an individual record.
#'
#' @returns A dataframe summarising an attribute/event on each row.
#' @export
df_indi_facts <- function(x, xref){
  check_indi_rec(x, xref)
  
  indi <- suppressWarnings(pull_record(x, xref))
  fcts <- indi@facts
  
  df <- data.frame(
    xref = xref,
    type = vapply(fcts, \(fct) chronify(fct@fact_type), FUN.VALUE = character(1)),
    val = vapply(fcts, \(fct) chronify(fct@fact_val), FUN.VALUE = character(1)),
    desc = vapply(fcts, \(fct) chronify(fct@fact_desc), FUN.VALUE = character(1)),
    date = vapply(fcts, \(fct) chronify(fct@FACT_DATE), FUN.VALUE = character(1)),
    place = vapply(fcts, \(fct) chronify(fct@FACT_LOCATION), FUN.VALUE = character(1)),
    age = vapply(fcts, \(fct) chronify(fct@age), FUN.VALUE = character(1))
  )
  
  indi_facts <- c(val_individual_attribute_types(TRUE),
                  val_individual_event_types(TRUE))
  df$type <- names(indi_facts)[match(df$type, indi_facts)]
  df
}

#' Summarise a family's attributes/events in a dataframe
#'
#' @param x A gedcom object.
#' @param xref The cross-reference identifier of a family record.
#'
#' @returns A dataframe summarising an attribute/event on each row.
#' @export
df_fam_facts <- function(x, xref){
  check_fam_rec(x, xref)
  
  fam <- suppressWarnings(pull_record(x, xref))
  fcts <- fam@facts
  
  df <- data.frame(
    xref = xref,
    type = vapply(fcts, \(fct) chronify(fct@fact_type), FUN.VALUE = character(1)),
    val = vapply(fcts, \(fct) chronify(fct@fact_val), FUN.VALUE = character(1)),
    desc = vapply(fcts, \(fct) chronify(fct@fact_desc), FUN.VALUE = character(1)),
    date = vapply(fcts, \(fct) chronify(fct@FACT_DATE), FUN.VALUE = character(1)),
    place = vapply(fcts, \(fct) chronify(fct@FACT_LOCATION), FUN.VALUE = character(1)),
    husb_age = vapply(fcts, \(fct) chronify(fct@husb_age), FUN.VALUE = character(1)),
    wife_age = vapply(fcts, \(fct) chronify(fct@wife_age), FUN.VALUE = character(1))
  )
  
  fam_facts <- c(val_family_attribute_types(TRUE),
                  val_family_event_types(TRUE))
  df$type <- names(fam_facts)[match(df$type, fam_facts)]
  df
}

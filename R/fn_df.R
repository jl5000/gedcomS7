

#' Summarise records of a particular type in a dataframe
#'
#' @param x A gedcom object.
#'
#' @return A dataframe summarising a record on each row.
#' @export
df_indi <- function(x){
  if(length(x@indi) == 0) return(NULL)
  
  df <- data.frame(
    xref = names(x@indi),
    name = desc_indi_name(x, names(x@indi), unnamed = ""),
    sex = vapply(x@indi, \(lines) chronify(find_ged_values(lines, "SEX")), FUN.VALUE = character(1)),
    birth_date = vapply(x@indi, \(lines) chronify(find_ged_values(lines, c("BIRT","DATE"))), FUN.VALUE = character(1)),
    birth_place = vapply(x@indi, \(lines) chronify(find_ged_values(lines, c("BIRT","PLAC"))), FUN.VALUE = character(1)),
    is_alive = vapply(names(x@indi), \(xref) is_alive(x, xref), FUN.VALUE = logical(1)),
    death_date = vapply(x@indi, \(lines) chronify(find_ged_values(lines, c("DEAT","DATE"))), FUN.VALUE = character(1)),
    death_place = vapply(x@indi, \(lines) chronify(find_ged_values(lines, c("DEAT","PLAC"))), FUN.VALUE = character(1)),
    fam_as_child = vapply(names(x@indi), \(xref) get_fam_as_child(x, xref, "BIRTH") |> 
                            paste(collapse = ";"), FUN.VALUE = character(1)),
    fam_as_spouse = vapply(names(x@indi), \(xref) get_fam_as_spouse(x, xref) |> 
                             paste(collapse = ";"), FUN.VALUE = character(1)),
    last_modified = vapply(x@indi, \(lines) chronify(find_ged_values(lines, c("CHAN","DATE"))), FUN.VALUE = character(1))
  )
  
  rownames(df) <- NULL
  df
}

#' @rdname df_indi
#' @export
df_fam <- function(x){
  if(length(x@fam) == 0) return(NULL)
  
  df <- data.frame(
    xref = names(x@fam),
    husb_xref = vapply(x@fam, \(lines) chronify(find_ged_values(lines, "HUSB")), FUN.VALUE = character(1)),
    wife_xref = vapply(x@fam, \(lines) chronify(find_ged_values(lines, "WIFE")), FUN.VALUE = character(1)),
    chil_xref = vapply(x@fam, \(lines) find_ged_values(lines, "CHIL") |>
                          paste(collapse = ";"), FUN.VALUE = character(1)),
    marr_date = vapply(x@fam, \(lines) chronify(find_ged_values(lines, c("MARR","DATE"))), FUN.VALUE = character(1)),
    marr_place = vapply(x@fam, \(lines) chronify(find_ged_values(lines, c("MARR","PLAC"))), FUN.VALUE = character(1)),
    last_modified = vapply(x@fam, \(lines) chronify(find_ged_values(lines, c("CHAN","DATE"))), FUN.VALUE = character(1))
  )
  
  rownames(df) <- NULL
  df
}

#' @rdname df_indi
#' @export
df_sour <- function(x){
  if(length(x@sour) == 0) return(NULL)
  
  df <- data.frame(
    xref = names(x@sour),
    originator = vapply(x@sour, \(lines) chronify(find_ged_values(lines, "AUTH")), FUN.VALUE = character(1)),
    title = vapply(x@sour, \(lines) chronify(find_ged_values(lines, "TITL")), FUN.VALUE = character(1)),
    repo_xref = vapply(x@sour, \(lines) find_ged_values(lines, "REPO") |>
                         paste(collapse = ";"), FUN.VALUE = character(1)),
    last_modified = vapply(x@sour, \(lines) chronify(find_ged_values(lines, c("CHAN","DATE"))), FUN.VALUE = character(1))
  )
  
  rownames(df) <- NULL
  df
}

#' @rdname df_indi
#' @export
df_repo <- function(x){
  if(length(x@repo) == 0) return(NULL)
  
  df <- data.frame(
    xref = names(x@repo),
    name = vapply(x@repo, \(lines) chronify(find_ged_values(lines, "NAME")), FUN.VALUE = character(1)),
    address = vapply(x@repo, \(lines) chronify(find_ged_values(lines, "ADDR")), FUN.VALUE = character(1)),
    last_modified = vapply(x@repo, \(lines) chronify(find_ged_values(lines, c("CHAN","DATE"))), FUN.VALUE = character(1))
  )
  
  rownames(df) <- NULL
  df
}

#' @rdname df_indi
#' @export
df_media <- function(x){
  if(length(x@media) == 0) return(NULL)
  
  df <- data.frame(
    xref = names(x@media),
    num_files = vapply(x@media, \(lines) length(find_ged_values(lines, "FILE")), FUN.VALUE = integer(1)),
    paths = vapply(x@media, \(lines) find_ged_values(lines, "FILE") |>
                     paste(collapse = ";"), FUN.VALUE = character(1)),
    last_modified = vapply(x@media, \(lines) chronify(find_ged_values(lines, c("CHAN","DATE"))), FUN.VALUE = character(1))
  )
  
  rownames(df) <- NULL
  df
}

#' @rdname df_indi
#' @export
df_note <- function(x){
  if(length(x@note) == 0) return(NULL)
  
  df <- data.frame(
    xref = names(x@note),
    language = vapply(x@note, \(lines) chronify(find_ged_values(lines, "LANG")), FUN.VALUE = character(1)),
    last_modified = vapply(x@note, \(lines) chronify(find_ged_values(lines, c("CHAN","DATE"))), FUN.VALUE = character(1))
  )
  
  rownames(df) <- NULL
  df
}

#' @rdname df_indi
#' @export
df_subm <- function(x){
  if(length(x@subm) == 0) return(NULL)
  
  df <- data.frame(
    xref = names(x@subm),
    name = vapply(x@subm, \(lines) chronify(find_ged_values(lines, "NAME")), FUN.VALUE = character(1)),
    address = vapply(x@subm, \(lines) chronify(find_ged_values(lines, "ADDR")), FUN.VALUE = character(1)),
    last_modified = vapply(x@subm, \(lines) chronify(find_ged_values(lines, c("CHAN","DATE"))), FUN.VALUE = character(1))
  )
  
  rownames(df) <- NULL
  df
}

#' Summarise an individual's attributes/events in a dataframe
#'
#' @param x A gedcom object.
#' @param xref The cross-reference identifier of an individual record.
#'
#' @return A dataframe summarising an attribute/event on each row.
#' @export
df_indi_facts <- function(x, xref){
  check_indi_rec(x, xref)
  
  indi <- pull_record(x, xref)
  fcts <- indi@facts
  
  df <- data.frame(
    xref = xref,
    type = vapply(fcts, \(fct) chronify(fct@fact_type), FUN.VALUE = character(1)),
    val = vapply(fcts, \(fct) chronify(fct@fact_val), FUN.VALUE = character(1)),
    desc = vapply(fcts, \(fct) chronify(fct@fact_desc), FUN.VALUE = character(1)),
    date = vapply(fcts, \(fct) chronify(fct@c_fact_date), FUN.VALUE = character(1)),
    place = vapply(fcts, \(fct) chronify(fct@c_fact_location), FUN.VALUE = character(1)),
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
#' @return A dataframe summarising an attribute/event on each row.
#' @export
df_fam_facts <- function(x, xref){
  check_fam_rec(x, xref)
  
  fam <- pull_record(x, xref)
  fcts <- fam@facts
  
  df <- data.frame(
    xref = xref,
    type = vapply(fcts, \(fct) chronify(fct@fact_type), FUN.VALUE = character(1)),
    val = vapply(fcts, \(fct) chronify(fct@fact_val), FUN.VALUE = character(1)),
    desc = vapply(fcts, \(fct) chronify(fct@fact_desc), FUN.VALUE = character(1)),
    date = vapply(fcts, \(fct) chronify(fct@c_fact_date), FUN.VALUE = character(1)),
    place = vapply(fcts, \(fct) chronify(fct@c_fact_location), FUN.VALUE = character(1)),
    husb_age = vapply(fcts, \(fct) chronify(fct@husb_age), FUN.VALUE = character(1)),
    wife_age = vapply(fcts, \(fct) chronify(fct@wife_age), FUN.VALUE = character(1))
  )
  
  fam_facts <- c(val_family_attribute_types(TRUE),
                  val_family_event_types(TRUE))
  df$type <- names(fam_facts)[match(df$type, fam_facts)]
  df
}

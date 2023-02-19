
#' Identify all partners in a Family Group
#'
#' @param x A gedcom object.
#' @param xref The xref of a Family Group record.
#'
#' @return A character vector of partner xrefs.
#' @export
get_fam_partners <- function(x, xref){
  
  if(!is_fam_uid(x, xref)) stop("The xref is not a Family Group record.")
  
  find_ged_values(x@famg[[xref]], "HUSB|WIFE")
}

#' Identify all children in a Family Group
#'
#' @param x A gedcom object.
#' @param xref The xref of a Family Group record.
#' @param birth_only Whether to only return biological children.
#'
#' @return A character vector of children xrefs.
#' @export
get_fam_children <- function(x, 
                              xref,
                              birth_only = FALSE){
  
  if(!is_fam_uid(x, xref)) stop("The xref is not a Family Group record.")
  
  famg_ged <- x@famg[[xref]]
  all_chil_xref <- find_ged_values(famg_ged, "CHIL")
  
  if(!birth_only) return(all_chil_xref)
  
  birth_chil_xref <- character()
  for(chil_xref in all_chil_xref){
    chil_ged <- x@indi[[chil_xref]]
    links <- extract_family_links(chil_ged)
    
    for(lnk in links){
      if(lnk@xref == xref && is_birth_child_link(lnk)){
        birth_chil_xref <- c(birth_chil_xref, chil_xref)
        break
      }
    }
    
  }
  birth_chil_xref
}

#' Identify all families for an individual where they are a child
#'
#' @param x A gedcom object.
#' @param xref The xref of an Individual record.
#' @param birth_only Whether to only return the family containing the biological parents.
#'
#' @return A character vector of family xrefs.
#' @export
get_fam_as_child <- function(x, 
                              xref,
                              birth_only = FALSE){
  
  if(!is_indi_uid(x, xref)) stop("The xref is not an Individual record.")
  
  indi_ged <- x@indi[[xref]]
  all_fam_xref <- find_ged_values(indi_ged, "FAMC")
  
  if(!birth_only) return(all_fam_xref)
  
  links <- extract_family_links(indi_ged)
  
  famc_xref <- character()
  for(lnk in links){
    if(is_birth_child_link(lnk)) famc_xref <- c(famc_xref, lnk@xref)
  }
  famc_xref
}

#' Identify all families for an individual where they are a partner
#' 
#' @param x A gedcom object.
#' @param xref The xref of an Individual record.
#'
#' @return A character vector of family xrefs.
#' @export
get_fam_as_partner <- function(x, xref){
  
  if(!is_indi_uid(x, xref)) stop("The xref is not an Individual record.")
  
  indi_ged <- x@indi[[xref]]
  
  find_ged_values(indi_ged, "FAMS")
}


#' Identify all partners for an individual
#'
#' @param x A gedcom object.
#' @param xref The xref of an Individual record.
#'
#' @return A character vector of partner xrefs.
#' @export
get_indi_partners <- function(x, xref){
  
  if(!is_indi_uid(x, xref)) stop("The xref is not an Individual record.")
  
  fams_xref <- get_fam_as_partner(x, xref)
  
  spou_xref <- lapply(fams_xref, \(fam) get_fam_partners(x, fam)) |>
    unlist()
  if(is.null(spou_xref)) return(character())
  
  spou_xref[spou_xref != xref]
}

#' Identify all children for an individual
#'
#' @param x A gedcom object.
#' @param xref The xref of an Individual record.
#' @param birth_only Whether to only return biological children.
#'
#' @return A character vector of children xrefs.
#' @export
get_indi_children <- function(x, 
                              xref,
                              birth_only = FALSE){
  
  if(!is_indi_uid(x, xref)) stop("The xref is not an Individual record.")
  
  fams_xref <- get_fam_as_partner(x, xref)
  
  chil_xref <- lapply(fams_xref, \(fam) get_fam_children(x, fam, birth_only)) |>
    unlist()
  if(is.null(chil_xref)) return(character())
  
  unique(chil_xref)
}

#' Identify all parents for an individual
#'
#' @param x A gedcom object.
#' @param xref The xref of an Individual record.
#' @param birth_only Whether to only return biological parents.
#'
#' @return A character vector of parent xrefs.
#' @export
get_indi_parents <- function(x, 
                             xref,
                             birth_only = FALSE){
  
  if(!is_indi_uid(x, xref)) stop("The xref is not an Individual record.")
  
  famc_xref <- get_fam_as_child(x, xref, birth_only)
  
  spou_xref <- lapply(famc_xref, \(fam) get_fam_partners(x, fam)) |>
    unlist()
  if(is.null(spou_xref)) return(character())
  
  unique(spou_xref)
}


#' Identify all mothers for an individual
#'
#' @param x A gedcom object.
#' @param xref The xref of an Individual record.
#' @param birth_only Whether to only return the biological mother.
#'
#' @return A character vector of mother xrefs.
#' @export
get_indi_mothers <- function(x,
                             xref,
                             birth_only = FALSE){
  
  get_indi_parents_fathmoth(x, xref, birth_only, FALSE)
}

#' Identify all fathers for an individual
#'
#' @param x A gedcom object.
#' @param xref The xref of an Individual record.
#' @param birth_only Whether to only return the biological father.
#'
#' @return A character vector of father xrefs.
#' @export
get_indi_fathers <- function(x,
                             xref,
                             birth_only = FALSE){
  
  get_indi_parents_fathmoth(x, xref, birth_only, TRUE)
}

get_indi_parents_fathmoth <- function(x,
                                      xref,
                                      birth_only = FALSE,
                                      father = TRUE){
  
  famc_xref <- get_fam_as_child(x, xref, birth_only)
  if(father) tag <- "HUSB" else tag <- "WIFE"
  
  spou_xref <- character()
  for(xref in famc_xref){
    spou_xref <- c(
      spou_xref,
      find_ged_values(x@famg[[xref]], tag)
    )
  }
  unique(spou_xref)
}

#' Identify all siblings for an individual
#'
#' @param x A gedcom object.
#' @param xref The xref of an Individual record.
#' @param birth_only Whether to only return biological siblings.
#' @param inc_half Whether to include siblings that only share one parent.
#'
#' @return A character vector of sibling xrefs.
#' @export
get_indi_siblings <- function(x, 
                              xref,
                              birth_only = FALSE,
                              inc_half = FALSE){
  
  if(!is_indi_uid(x, xref)) stop("The xref is not an Individual record.")
  
  if(inc_half){
    spou_xref <- get_indi_parents(x, xref, birth_only)
    
    sibs_xref <- lapply(spou_xref, \(par) get_indi_children(x, par, birth_only)) |>
      unlist()
    
  } else {
    famc_xref <- get_fam_as_child(x, xref, birth_only)
    
    sibs_xref <- lapply(famc_xref, \(fam) get_fam_children(x, fam, birth_only)) |>
      unlist()
  }
  
  if(is.null(sibs_xref)) return(character())
  unique(sibs_xref[sibs_xref != xref])
}

#' Identify all cousins for an individual
#'
#' @param x A gedcom object.
#' @param xref The xref of an Individual record.
#' @param degree Whether to return first cousins (degree = 1), second cousins (degree = 2), etc.
#' @param inc_half Whether to include half cousins.
#'
#' @return A character vector of cousin xrefs.
#' @export
get_indi_cousins <- function(x, 
                             xref,
                             degree = 1,
                             inc_half = FALSE){
  
  if(!is_indi_uid(x, xref)) stop("The xref is not an Individual record.")
  if(degree < 1) stop("The degree must be at least 1.")
  degree <- floor(degree)
  
  pars_xref <- xref
  for(i in seq_len(degree)){
    pars_xref <- lapply(pars_xref, \(par) get_indi_parents(x, par, birth_only = TRUE)) |>
      unlist()
  }
  
  sibs_xref <- lapply(pars_xref, \(par) get_indi_siblings(x, par, birth_only = TRUE, inc_half)) |>
    unlist()

  chil_xref <- sibs_xref
  for(i in seq_len(degree)){
    chil_xref <- lapply(chil_xref, \(ch) get_indi_children(x, ch, birth_only = TRUE)) |>
      unlist()
  }
  
  if(is.null(chil_xref)) return(character())
  chil_xref
}

#' Identify all supporting records for a set of records
#' 
#' This function gets all supporting records (and onwards dependencies) for a set of records. Supporting records
#' are note, multimedia, source, and repository records, i.e. those providing supporting evidence and comments.
#'
#' @param x A gedcom object.
#' @param xrefs The xrefs of records to get supporting records for.
#' @param inc_note Whether to include Note records.
#' @param inc_media Whether to include Multimedia records.
#' @param inc_sour Whether to include Source records.
#' @param inc_repo Whether to include Repository records.
#'
#' @return A character vector of supporting record xrefs.
#' @export
get_supporting_recs <- function(x, 
                                xrefs,
                                inc_note = TRUE,
                                inc_media = TRUE,
                                inc_sour = TRUE,
                                inc_repo = TRUE){
  if(length(xrefs) == 0) return(character())
  
  tags <- NULL
  if (inc_note) tags <- c(tags, "NOTE")
  if (inc_media) tags <- c(tags, "OBJE")
  if (inc_sour) tags <- c(tags, "SOUR")
  if (inc_repo) tags <- c(tags, "REPO")
  if(length(tags) == 0) return(character())
  
  vals <- lapply(xrefs, \(xref){
    rec_lines <- c(x@indi, x@famg, x@sour,
                   x@repo, x@media, x@note)[[xref]]
    
    tgs <- extract_ged_tag(rec_lines)
    extract_ged_value(rec_lines)[tgs %in% tags]
  }) |>
    unlist()
  
  links <- unique(vals[grepl(reg_xref(TRUE), vals)])

  unique(
    c(links,
      get_supporting_recs(x, links, inc_note, inc_media, inc_sour, inc_repo))
  )
}

#' Identify unreferenced records
#' 
#' This function identifies records that are not referenced in any other records.
#' 
#' @details You would expect every record to be referenced by another in some way. For example, Individual
#' records should reference Family Group records (and vice-versa), Repository records should be referenced
#' by Source records, and Source records should be cited by other records.
#'
#' @param x A gedcom object.
#'
#' @return A character vector of xrefs that are not referenced anywhere else in the gedcom object.
#' @export
get_unused_recs <- function(x){
  
  ged <- x@as_ged
  
  xrefs <- unlist(x@xrefs)
  vals <- extract_ged_value(ged)
  xref_vals <- vals[grep(reg_xref(TRUE), vals)]

  setdiff(xrefs, xref_vals)  
}

#' Identify all descendants for an individual
#' 
#' This function identifies records in an entire branch of the family tree below a certain individual.
#' 
#' @param x A gedcom object.
#' @param xref The xref of an Individual record.
#' @param inc_indi Whether to also include the individual themselves.
#' @param inc_part Whether to also include all partners of this individual (and their descendants and
#' descendants' partners).
#' @param inc_famg Whether to also include all Family Group records where this individual is a partner 
#' (and all descendants' Family Group records).
#' @param inc_supp Whether to also include all supporting records (Note, Source, Repository, Multimedia).
#' @param birth_only Whether to only include biological descendants.
#'
#' @return A vector of xrefs of descendants.
#' @export
get_descendants <- function(x, 
                            xref,
                            inc_indi = FALSE,
                            inc_part = FALSE,
                            inc_famg = FALSE,
                            inc_supp = FALSE,
                            birth_only = TRUE){
  
  if(!is_indi_uid(x, xref)) stop("The xref is not an Individual record.")
  
  return_xrefs <- character()
  
  spou_xref <- get_indi_partners(x, xref)
  chil_xref <- get_indi_children(x, xref, birth_only)
  fams_xref <- get_fam_as_partner(x, xref)
  
  # if partner is to be included, add their children to be included
  if (inc_part) {
    part_chil_xref <- lapply(spou_xref, \(par) get_indi_children(x, par, birth_only)) |> 
      unlist()
    
    chil_xref <- unique(c(chil_xref, part_chil_xref))
  }
  
  #deal with family groups first (while the individuals are still in them)
  if (inc_famg) return_xrefs <- c(return_xrefs, fams_xref)
  if (inc_part) return_xrefs <- c(return_xrefs, spou_xref)
  if (inc_indi) return_xrefs <- c(return_xrefs, xref)
  
  # identify children
  for(i in seq_along(chil_xref)) {
    return_xrefs <- c(return_xrefs,
                      get_descendants(x, chil_xref[i], TRUE, inc_part, inc_famg, FALSE, birth_only))
  }
  
  # only get supporting records if this is the top level call
  if (inc_supp && length(as.character(sys.call())) == 8 && 
      any(as.character(sys.call()) != c("get_descendants","x","chil_xref[i]","TRUE",
                                        "inc_part","inc_famg","FALSE","birth_only"))){
    
    c(return_xrefs,
      get_supporting_recs(x, return_xrefs))
  } else {
    return_xrefs
  }
}

#' Identify all ancestors for an individual
#' 
#' This function identifies records in an entire branch of the family tree above a certain individual.
#' 
#' @param x A gedcom object.
#' @param xref The xref of an Individual record.
#' @param inc_indi Whether to also include the individual themselves.
#' @param inc_sibs Whether to also include all siblings of ancestors (siblings of this individual will only be
#' included if the individual is included).
#' @param inc_famg Whether to also include all Family Group records where this individual is a child 
#' (and all ancestors' Family Group records).
#' @param inc_supp Whether to also include all supporting records (Note, Source, Repository, Multimedia).
#' @param birth_only Whether to only include biological ancestors.
#'
#' @return A vector of xrefs of ancestors.
#' @export
get_ancestors <- function(x, 
                          xref,
                          inc_indi = FALSE,
                          inc_sibs = FALSE,
                          inc_famg = FALSE,
                          inc_supp = FALSE,
                          birth_only = TRUE){
  
  if(!is_indi_uid(x, xref)) stop("The xref is not an Individual record.")
  
  return_xrefs <- character()
  
  sibs_xref <- get_indi_siblings(x, xref, birth_only)
  pars_xref <- get_indi_parents(x, xref, birth_only)
  famc_xref <- get_fam_as_child(x, xref, birth_only)
  
  if (inc_indi & inc_sibs) {
    sib_par_xref <- lapply(sibs_xref, \(sib) get_indi_parents(x, sib, birth_only)) |>
      unlist()
    
    pars_xref <- unique(c(pars_xref, sib_par_xref))
  }
  
  if (inc_famg) return_xrefs <- c(return_xrefs, famc_xref)
  if (inc_indi & inc_sibs) return_xrefs <- c(return_xrefs, sibs_xref)
  if (inc_indi) return_xrefs <- c(return_xrefs, xref)
  
  for(i in seq_along(pars_xref)) {
    return_xrefs <- c(return_xrefs,
                      get_ancestors(x, pars_xref[i], TRUE, inc_sibs, inc_famg, FALSE, birth_only))
  }
  
  # only get supporting records if this is the top level call
  if (inc_supp && length(as.character(sys.call())) == 8 && 
      any(as.character(sys.call()) != c("get_ancestors","x","pars_xref[i]","TRUE",
                                        "inc_sibs","inc_famg","FALSE"))) {
    
    c(return_xrefs,
      get_supporting_recs(x, return_xrefs))
  } else {
    return_xrefs
  }
}


describe_indi = function(x, xrefs){
  nms = character()
  for(xref in xrefs){
    nms = c(nms, pull_record(x,xref)@primary_name)
  }
  nms
}

#' Identify all partners in a Family Group
#'
#' @param x A gedcom object.
#' @param xref The xref of a Family Group record.
#'
#' @return A character vector of partner xrefs.
#' @export
get_famg_partners <- function(x, xref){
  
  if(!is_famg_xref(x, xref)) stop("The xref is not a Family Group record.")
  
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
get_famg_children <- function(x, 
                              xref,
                              birth_only = FALSE){
  
  if(!is_famg_xref(x, xref)) stop("The xref is not a Family Group record.")
  
  famg_rec <- x@famg[[xref]]
  all_chil <- find_ged_values(famg_rec, "CHIL")
  
  if(!birth_only) return(all_chil)
  
  birth_chil <- character()
  for(chil in all_chil){
    chil_rec <- x@indi[[chil]]
    links <- extract_family_links(chil_rec)
    
    for(lnk in links){
      if(lnk@xref == xref && is_birth_child_link(lnk)){
        birth_chil <- c(birth_chil, chil)
        break
      }
    }
    
  }
  birth_chil
}

#' Identify all families for an individual where they are a child
#'
#' @param x A gedcom object.
#' @param xref The xref of an Individual record.
#' @param birth_only Whether to only return the family containing the biological parents.
#'
#' @return A character vector of family xrefs.
#' @export
get_famg_as_child <- function(x, 
                              xref,
                              birth_only = FALSE){
  
  if(!is_indi_xref(x, xref)) stop("The xref is not an Individual record.")
  
  indi_rec <- x@indi[[xref]]
  all_fam <- find_ged_values(indi_rec, "FAMC")
  
  if(!birth_only) return(all_fam)
  
  links <- extract_family_links(indi_rec)
  
  famg <- character()
  for(lnk in links){
    if(is_birth_child_link(lnk)) famg <- c(famg, lnk@xref)
  }
  famg
}

#' Identify all families for an individual where they are a partner
#' 
#' @param gedcom A gedcom object.
#' @param xref The xref of an Individual record.
#'
#' @return A character vector of family xrefs.
#' @export
get_famg_as_partner <- function(x, xref){
  
  if(!is_indi_xref(x, xref)) stop("The xref is not an Individual record.")
  
  indi_rec <- x@indi[[xref]]
  
  find_ged_values(indi_rec, "FAMS")
}


#' Identify all partners for an individual
#'
#' @param x A gedcom object.
#' @param xref The xref of an Individual record.
#'
#' @return A character vector of partner xrefs.
#' @export
get_indi_partners <- function(x, xref){
  
  if(!is_indi_xref(x, xref)) stop("The xref is not an Individual record.")
  
  fams <- get_famg_as_partner(x, xref)
  
  parts <- lapply(fams, \(fam) get_famg_partners(x, fam)) |>
    unlist()
  if(is.null(parts)) return(character())
  
  parts[parts != xref]
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
  
  if(!is_indi_xref(x, xref)) stop("The xref is not an Individual record.")
  
  fams <- get_famg_as_partner(x, xref)
  
  chil <- lapply(fams, \(fam) get_famg_children(x, fam, birth_only)) |>
    unlist()
  if(is.null(chil)) return(character())
  
  unique(chil)
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
  
  if(!is_indi_xref(x, xref)) stop("The xref is not an Individual record.")
  
  famc <- get_famg_as_child(x, xref, birth_only)
  
  pars <- lapply(famc, \(fam) get_famg_partners(x, fam)) |>
    unlist()
  if(is.null(pars)) return(character())
  
  unique(pars)
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
  
  if(!is_indi_xref(x, xref)) stop("The xref is not an Individual record.")
  
  sibs <- character()
  if(inc_half){
    pars <- get_indi_parents(x, xref, birth_only)
    
    sibs <- lapply(pars, \(par) get_indi_children(x, par, birth_only)) |>
      unlist()
    
  } else {
    famc <- get_famg_as_child(x, xref, birth_only)
    
    sibs <- lapply(famc, \(fam) get_famg_children(x, fam, birth_only)) |>
      unlist()
  }
  
  if(is.null(sibs)) return(character())
  unique(sibs[sibs != xref])
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
  
  if(!is_indi_xref(x, xref)) stop("The xref is not an Individual record.")
  if(degree < 1) stop("The degree must be at least 1.")
  degree <- floor(degree)
  
  pars <- xref
  for(i in seq_len(degree)){
    pars <- lapply(pars, \(par) get_indi_parents(x, par, birth_only = TRUE)) |>
      unlist()
  }
  
  sibs <- lapply(pars, \(par) get_indi_siblings(x, par, birth_only = TRUE, inc_half)) |>
    unlist()

  chil <- sibs
  for(i in seq_len(degree)){
    chil <- lapply(chil, \(ch) get_indi_children(x, ch, birth_only = TRUE)) |>
      unlist()
  }
  
  if(is.null(chil)) return(character())
  chil
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
  
  if(!is_indi_xref(x, xref)) stop("The xref is not an Individual record.")
  
  return_xrefs <- character()
  
  part <- get_indi_partners(x, xref)
  chil <- get_indi_children(x, xref, birth_only)
  fams <- get_famg_as_partner(x, xref)
  
  # if partner is to be included, add their children to be included
  if (inc_part) {
    part_chil <- lapply(part, \(par) get_indi_children(x, par, birth_only)) |> 
      unlist()
    
    chil <- unique(c(chil, part_chil))
  }
  
  #deal with family groups first (while the individuals are still in them)
  if (inc_famg) return_xrefs <- c(return_xrefs, fams)
  if (inc_part) return_xrefs <- c(return_xrefs, part)
  if (inc_indi) return_xrefs <- c(return_xrefs, xref)
  
  # identify children
  for(i in seq_along(chil)) {
    return_xrefs <- c(return_xrefs,
                      get_descendants(x, chil[i], TRUE, inc_part, inc_famg, FALSE, birth_only))
  }
  
  # only get supporting records if this is the top level call
  if (inc_supp && length(as.character(sys.call())) == 8 && 
      any(as.character(sys.call()) != c("get_descendants","x","chil[i]","TRUE",
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
  
  if(!is_indi_xref(x, xref)) stop("The xref is not an Individual record.")
  
  return_xrefs <- character()
  
  sibs <- get_indi_siblings(x, xref, birth_only)
  pars <- get_indi_parents(x, xref, birth_only)
  famc <- get_famg_as_child(x, xref, birth_only)
  
  if (inc_indi & inc_sibs) {
    sib_par <- lapply(sibs, \(sib) get_indi_parents(x, sib, birth_only)) |>
      unlist()
    
    pars <- unique(c(pars, sib_par))
  }
  
  if (inc_famg) return_xrefs <- c(return_xrefs, famc)
  if (inc_indi & inc_sibs) return_xrefs <- c(return_xrefs, sibs)
  if (inc_indi) return_xrefs <- c(return_xrefs, xref)
  
  for(i in seq_along(pars)) {
    return_xrefs <- c(return_xrefs,
                      get_ancestors(x, pars[i], TRUE, inc_sibs, inc_famg, FALSE, birth_only))
  }
  
  # only get supporting records if this is the top level call
  if (inc_supp && length(as.character(sys.call())) == 8 && 
      any(as.character(sys.call()) != c("get_ancestors","x","pars[i]","TRUE",
                                        "inc_sibs","inc_famg","FALSE"))) {
    
    c(return_xrefs,
      get_supporting_recs(x, return_xrefs))
  } else {
    return_xrefs
  }
}

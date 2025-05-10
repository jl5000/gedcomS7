
#' Identify all families for an individual where they are a child
#'
#' @param x A gedcom object.
#' @param xref The xref of an Individual record.
#' @param pedigrees A character vector of allowed family-child linkages. 
#' By default, NULL means all pedigrees (e.g. inc. ADOPTED). 
#' If it includes "BIRTH" then this will also pick up non-existent values 
#' (since BIRTH is assumed to be the default).
#'
#' @returns A character vector of xrefs.
#' @export
get_fam_as_child <- function(x, 
                             xref,
                             pedigrees = NULL){
  
  check_indi_rec(x, xref)
  
  indi_ged <- x@records@RAW@INDI[[xref]]
  all_fam_xref <- find_ged_values(indi_ged, "FAMC") |> 
    remove_void_xrefs()
  
  if(is.null(pedigrees)) return(all_fam_xref)
  
  chil_links <- parse_family_links(indi_ged, as_spouse = FALSE)
  
  famc_xref <- character()
  for(lnk in chil_links){
    if(pedigree_in_set(lnk@pedigree, pedigrees))
      famc_xref <- c(famc_xref, lnk@fam_xref)
  }
  famc_xref
}

#' Identify all families for an individual where they are a partner
#' 
#' @inheritParams get_fam_as_child
#'
#' @inherit get_fam_as_child return
#' @export
get_fam_as_spouse <- function(x, xref){
  
  check_indi_rec(x, xref)
  
  indi_ged <- x@records@RAW@INDI[[xref]]
  
  find_ged_values(indi_ged, "FAMS") |> 
    remove_void_xrefs()
}


#' Identify all partners in a family
#'
#' @inheritParams get_fam_as_child
#' @param xref The xref of a Family record.
#'
#' @inherit get_fam_as_child return
#' @export
get_fam_partners <- function(x, xref){
  
  check_fam_rec(x, xref)
  
  find_ged_values(x@records@RAW@FAM[[xref]], "HUSB|WIFE") |> 
    remove_void_xrefs()
}

#' Identify all children in a family
#'
#' @inheritParams get_fam_as_child
#' @param xref The xref of a Family record.
#'
#' @inherit get_fam_as_child return
#' @export
#' @tests
#' expect_equal(get_fam_children(test_ged(), "@F1@"), "@I1@")
#' expect_equal(get_fam_children(test_ged(), "@F1@", "BIRTH"), "@I1@")
#' expect_equal(get_fam_children(test_ged(), "@F2@"), c("@I5@", "@I6@"))
get_fam_children <- function(x, 
                              xref,
                              pedigrees = NULL){
  
  check_fam_rec(x, xref)
  
  fam_ged <- x@records@RAW@FAM[[xref]]
  all_chil_xref <- find_ged_values(fam_ged, "CHIL") |> 
    remove_void_xrefs()
  
  if(is.null(pedigrees)) return(all_chil_xref)
  
  ped_chil_xref <- character()
  for(chil_xref in all_chil_xref){
    chil_ged <- x@records@RAW@INDI[[chil_xref]]
    chil_links <- parse_family_links(chil_ged, as_spouse = FALSE)
    
    for(lnk in chil_links){
      if(lnk@fam_xref == xref && 
         pedigree_in_set(lnk@pedigree, pedigrees)){
        
        ped_chil_xref <- c(ped_chil_xref, chil_xref)
        break
      }
    }
    
  }
  ped_chil_xref
}



#' Identify all partners for an individual
#'
#' @inheritParams get_fam_as_child
#'
#' @inherit get_fam_as_child return
#' @export
#' @tests
#' expect_equal(get_indi_partners(test_ged(), "@I1@"), "@I4@")
#' expect_equal(get_indi_partners(test_ged(), "@I4@"), "@I1@")
get_indi_partners <- function(x, xref){
  
  check_indi_rec(x, xref)
  
  fams_xref <- get_fam_as_spouse(x, xref)
  
  spou_xref <- lapply(fams_xref, \(fam) get_fam_partners(x, fam)) |>
    unlist()
  if(is.null(spou_xref)) return(character())
  
  spou_xref[spou_xref != xref]
}

#' Identify all children for an individual
#'
#' @inheritParams get_fam_as_child
#'
#' @inherit get_fam_as_child return
#' @export
#' @tests
#' expect_equal(get_indi_children(test_ged(), "@I1@"), c("@I5@", "@I6@"))
#' expect_equal(get_indi_children(test_ged(), "@I4@"), c("@I5@", "@I6@"))
#' expect_equal(get_indi_children(test_ged(), "@I2@"), "@I1@")
get_indi_children <- function(x, 
                              xref,
                              pedigrees = NULL){
  
  check_indi_rec(x, xref)
  
  fams_xref <- get_fam_as_spouse(x, xref)
  
  chil_xref <- lapply(fams_xref, \(fam) get_fam_children(x, fam, pedigrees)) |>
    unlist()
  if(is.null(chil_xref)) return(character())
  
  unique(chil_xref)
}

#' Identify all parents for an individual
#'
#' @inheritParams get_fam_as_child
#'
#' @inherit get_fam_as_child return
#' @export
#' @tests
#' expect_equal(get_indi_parents(test_ged(), "@I1@"), c("@I2@", "@I3@"))
#' expect_equal(get_indi_parents(test_ged(), "@I5@"), c("@I1@", "@I4@"))
#' expect_equal(get_indi_parents(test_ged(), "@I6@"), c("@I1@", "@I4@"))
#' expect_equal(get_indi_parents(test_ged(), "@I2@"), character())
get_indi_parents <- function(x, 
                             xref,
                             pedigrees = NULL){
  
  check_indi_rec(x, xref)
  
  famc_xref <- get_fam_as_child(x, xref, pedigrees)
  
  spou_xref <- lapply(famc_xref, \(fam) get_fam_partners(x, fam)) |>
    unlist()
  if(is.null(spou_xref)) return(character())
  
  unique(spou_xref)
}


#' Identify all mothers for an individual
#'
#' @inheritParams get_fam_as_child
#'
#' @inherit get_fam_as_child return
#' @export
#' @tests
#' expect_equal(get_indi_mothers(test_ged(), "@I1@"), "@I3@")
get_indi_mothers <- function(x,
                             xref,
                             pedigrees = NULL){
  
  get_indi_parents_fathmoth(x, xref, pedigrees, FALSE)
}

#' Identify all fathers for an individual
#'
#' @inheritParams get_fam_as_child
#'
#' @inherit get_fam_as_child return
#' @export
#' @tests
#' expect_equal(get_indi_fathers(test_ged(), "@I1@"), "@I2@")
get_indi_fathers <- function(x,
                             xref,
                             pedigrees = NULL){
  
  get_indi_parents_fathmoth(x, xref, pedigrees, TRUE)
}

get_indi_parents_fathmoth <- function(x,
                                      xref,
                                      pedigrees = NULL,
                                      father = TRUE){
  
  check_indi_rec(x, xref)
  
  famc_xref <- get_fam_as_child(x, xref, pedigrees)
  if(father) tag <- "HUSB" else tag <- "WIFE"
  
  spou_xref <- character()
  for(xref in famc_xref){
    spou_xref <- c(
      spou_xref,
      find_ged_values(x@records@RAW@FAM[[xref]], tag)
    )
  }
  unique(spou_xref) |> 
    remove_void_xrefs()
}

#' Identify all siblings for an individual
#'
#' @inheritParams get_fam_as_child
#' @param inc_half Whether to include siblings that only share one parent.
#'
#' @inherit get_fam_as_child return
#' @export
#' @tests
#' expect_equal(get_indi_siblings(test_ged(), "@I1@"), character())
#' expect_equal(get_indi_siblings(test_ged(), "@I5@"), "@I6@")
#' expect_equal(get_indi_siblings(test_ged(), "@I6@"), "@I5@")
#' expect_equal(get_indi_siblings(test_ged(), "@I6@", inc_half = TRUE), "@I5@")
get_indi_siblings <- function(x, 
                              xref,
                              pedigrees = NULL,
                              inc_half = FALSE){
  
  check_indi_rec(x, xref)
  
  if(inc_half){
    spou_xref <- get_indi_parents(x, xref, pedigrees)
    
    sibs_xref <- lapply(spou_xref, \(par) get_indi_children(x, par, pedigrees)) |>
      unlist()
    
  } else {
    famc_xref <- get_fam_as_child(x, xref, pedigrees)
    
    sibs_xref <- lapply(famc_xref, \(fam) get_fam_children(x, fam, pedigrees)) |>
      unlist()
  }
  
  if(is.null(sibs_xref)) return(character())
  unique(sibs_xref[sibs_xref != xref])
}

#' Identify all cousins for an individual
#'
#' @inheritParams get_fam_as_child
#' @param degree Whether to return first cousins (degree = 1), second cousins (degree = 2), etc.
#' @param inc_half Whether to include half cousins.
#'
#' @inherit get_fam_as_child return
#' @export
get_indi_cousins <- function(x, 
                             xref,
                             degree = 1,
                             inc_half = FALSE){
  
  check_indi_rec(x, xref)
  if(degree < 1) stop("The degree must be at least 1.")
  degree <- floor(degree)
  
  pars_xref <- xref
  for(i in seq_len(degree)){
    pars_xref <- lapply(pars_xref, \(par) get_indi_parents(x, par, "BIRTH")) |>
      unlist()
  }
  
  sibs_xref <- lapply(pars_xref, \(par) get_indi_siblings(x, par, "BIRTH", inc_half)) |>
    unlist()

  chil_xref <- sibs_xref
  for(i in seq_len(degree)){
    chil_xref <- lapply(chil_xref, \(ch) get_indi_children(x, ch, "BIRTH")) |>
      unlist()
  }
  
  if(is.null(chil_xref)) return(character())
  chil_xref
}

#' Identify all supporting records for a set of records
#' 
#' This function gets all supporting records (and onwards dependencies) for a set of records. Supporting records
#' are note, multimedia, source, submitter and repository records, i.e. those providing supporting evidence and comments.
#'
#' @param x A gedcom object.
#' @param xrefs The xrefs of records to get supporting records for.
#' @param inc_note Whether to include Note records.
#' @param inc_media Whether to include Multimedia records.
#' @param inc_sour Whether to include Source records.
#' @param inc_repo Whether to include Repository records.
#' @param inc_subm Whether to include Submitter records.
#'
#' @inherit get_fam_as_child return
#' @export
get_supporting_recs <- function(x, 
                                xrefs,
                                inc_note = TRUE,
                                inc_media = TRUE,
                                inc_sour = TRUE,
                                inc_repo = TRUE,
                                inc_subm = TRUE){
  if(length(xrefs) == 0) return(character())
  
  tags <- NULL
  if (inc_note) tags <- c(tags, "SNOTE")
  if (inc_media) tags <- c(tags, "OBJE")
  if (inc_sour) tags <- c(tags, "SOUR")
  if (inc_repo) tags <- c(tags, "REPO")
  if (inc_subm) tags <- c(tags, "SUBM")
  if(length(tags) == 0) return(character())
  
  vals <- lapply(xrefs, \(xref){
    rec_lines <- c(x@records@RAW@INDI, x@records@RAW@FAM, 
                   x@records@RAW@SOUR, x@records@RAW@REPO, x@records@RAW@SUBM,
                   x@records@RAW@OBJE, x@records@RAW@SNOTE)[[xref]]
    
    tgs <- parse_line_tag(rec_lines)
    parse_line_value(rec_lines)[tgs %in% tags]
  }) |>
    unlist()
  
  links <- unique(vals[grepl(reg_xref(TRUE), vals)])

  unique(
    c(links,
      get_supporting_recs(x, links, inc_note, inc_media, inc_sour, inc_repo, inc_subm))
  )
}

#' Identify unreferenced records
#' 
#' This function identifies records that are not referenced in any other records.
#' 
#' @details You would expect every record to be referenced by another in some way. For example, Individual
#' records should reference Family records (and vice-versa), Repository records should be referenced
#' by Source records, and Source records should be cited by other records.
#'
#' @inheritParams get_fam_as_child
#'
#' @inherit get_fam_as_child return
#' @export
#' @tests
#' expect_equal(get_unused_recs(test_ged()), c("@U2@","@F3@","@N1@"))
get_unused_recs <- function(x){
  
  ged <- x@GEDCOM
  
  xrefs <- unlist(x@records@XREFS)
  vals <- parse_line_value(ged)
  xref_vals <- vals[grep(reg_xref(TRUE), vals)]

  setdiff(xrefs, xref_vals)  
}

#' Identify all descendants for an individual
#' 
#' This function identifies records in an entire branch of the family tree below a certain individual.
#' 
#' @inheritParams get_fam_as_child
#' @param inc_indi Whether to also include the individual themselves.
#' @param inc_part Whether to also include all partners of this individual (and their descendants and
#' descendants' partners).
#' @param inc_fam Whether to also include all Family records where this individual is a partner 
#' (and all descendants' Family records).
#' @param inc_supp Whether to also include all supporting records (Note, Source, Repository, Multimedia).
#'
#' @inherit get_fam_as_child return
#' @export
#' @tests
#' expect_equal(sort(get_descendants(test_ged(), "@I2@")), 
#'              c("@I1@", "@I5@", "@I6@"))
#' expect_equal(sort(get_descendants(test_ged(), "@I2@", 
#'                                   inc_indi = TRUE)), 
#'              c("@I1@", "@I2@" ,"@I5@", "@I6@"))
#' expect_equal(sort(get_descendants(test_ged(), "@I2@", 
#'                                   inc_part = TRUE)), 
#'              c("@I1@", "@I3@", "@I4@", "@I5@", "@I6@"))
#' expect_equal(sort(get_descendants(test_ged(), "@I2@", 
#'                                   inc_fam = TRUE)), 
#'              c("@F1@", "@F2@", "@I1@", "@I5@", "@I6@"))
get_descendants <- function(x, 
                            xref,
                            inc_indi = FALSE,
                            inc_part = FALSE,
                            inc_fam = FALSE,
                            inc_supp = FALSE,
                            pedigrees = NULL){
  
  check_indi_rec(x, xref)
  
  return_xrefs <- character()
  
  spou_xref <- get_indi_partners(x, xref)
  chil_xref <- get_indi_children(x, xref, pedigrees)
  fams_xref <- get_fam_as_spouse(x, xref)
  
  # if partner is to be included, add their children to be included
  if (inc_part) {
    part_chil_xref <- lapply(spou_xref, \(par) get_indi_children(x, par, pedigrees)) |> 
      unlist()
    
    chil_xref <- unique(c(chil_xref, part_chil_xref))
  }
  
  #deal with families first (while the individuals are still in them)
  if (inc_fam) return_xrefs <- c(return_xrefs, fams_xref)
  if (inc_part) return_xrefs <- c(return_xrefs, spou_xref)
  if (inc_indi) return_xrefs <- c(return_xrefs, xref)
  
  # identify children
  for(i in seq_along(chil_xref)) {
    return_xrefs <- c(return_xrefs,
                      get_descendants(x, chil_xref[i], TRUE, inc_part, inc_fam, FALSE, pedigrees))
  }
  
  # only get supporting records if this is the top level call
  if (inc_supp && length(as.character(sys.call())) == 8 && 
      any(as.character(sys.call()) != c("get_descendants","x","chil_xref[i]","TRUE",
                                        "inc_part","inc_fam","FALSE","pedigrees"))){
    
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
#' @inheritParams get_fam_as_child
#' @param inc_indi Whether to also include the individual themselves.
#' @param inc_sibs Whether to also include all siblings of ancestors (siblings of this individual will only be
#' included if the individual is included).
#' @param inc_fam Whether to also include all Family records where this individual is a child 
#' (and all ancestors' Family records).
#' @param inc_supp Whether to also include all supporting records (Note, Source, Repository, Multimedia).
#'
#' @inherit get_fam_as_child return
#' @export
#' @tests
#' expect_equal(sort(get_ancestors(test_ged(), "@I5@")),
#'              c("@I1@","@I2@","@I3@","@I4@"))
#' expect_equal(sort(get_ancestors(test_ged(), "@I5@", inc_indi = TRUE)),
#'              c("@I1@","@I2@","@I3@","@I4@","@I5@"))
#' expect_equal(sort(get_ancestors(test_ged(), "@I5@", inc_sibs = TRUE)),
#'              c("@I1@","@I2@","@I3@","@I4@"))
#' expect_equal(sort(get_ancestors(test_ged(), "@I5@", 
#'                                 inc_indi = TRUE,
#'                                 inc_sibs = TRUE)),
#'              c("@I1@","@I2@","@I3@","@I4@","@I5@","@I6@"))
#' expect_equal(sort(get_ancestors(test_ged(), "@I5@", inc_fam = TRUE)),
#'              c("@F1@","@F2@","@I1@","@I2@","@I3@","@I4@"))
get_ancestors <- function(x, 
                          xref,
                          inc_indi = FALSE,
                          inc_sibs = FALSE,
                          inc_fam = FALSE,
                          inc_supp = FALSE,
                          pedigrees = NULL){
  
  check_indi_rec(x, xref)
  
  return_xrefs <- character()
  
  sibs_xref <- get_indi_siblings(x, xref, pedigrees)
  pars_xref <- get_indi_parents(x, xref, pedigrees)
  famc_xref <- get_fam_as_child(x, xref, pedigrees)
  
  if (inc_indi & inc_sibs) {
    sib_par_xref <- lapply(sibs_xref, \(sib) get_indi_parents(x, sib, pedigrees)) |>
      unlist()
    
    pars_xref <- unique(c(pars_xref, sib_par_xref))
  }
  
  if (inc_fam) return_xrefs <- c(return_xrefs, famc_xref)
  if (inc_indi & inc_sibs) return_xrefs <- c(return_xrefs, sibs_xref)
  if (inc_indi) return_xrefs <- c(return_xrefs, xref)
  
  for(i in seq_along(pars_xref)) {
    return_xrefs <- c(return_xrefs,
                      get_ancestors(x, pars_xref[i], TRUE, inc_sibs, inc_fam, FALSE, pedigrees))
  }
  
  # only get supporting records if this is the top level call
  if (inc_supp && length(as.character(sys.call())) == 8 && 
      any(as.character(sys.call()) != c("get_ancestors","x","pars_xref[i]","TRUE",
                                        "inc_sibs","inc_fam","FALSE"))) {
    
    c(return_xrefs,
      get_supporting_recs(x, return_xrefs))
  } else {
    return_xrefs
  }
}

pedigree_in_set <- function(pedigree, set){
  if(length(pedigree) == 0){
    return("BIRTH" %in% set)
  }
  pedigree %in% set
}

#' Identify all records that contain a pattern
#'
#' @inheritParams get_fam_as_child
#' @param pattern A regular expression. Case is ignored.
#' @param return_context Whether to return a named list of matching lines in
#' the GEDCOM (TRUE) or just the xrefs (FALSE, default).
#'
#' @returns Either a vector of matching xrefs, or a named list of matching GEDCOM lines.
#' @export
#' @tests
#' expect_equal(get_records_by_pattern(test_ged(), "California"), "@I1@")
#' expect_equal(get_records_by_pattern(test_ged(), "Mother"), "@I3@")
#' expect_equal(get_records_by_pattern(test_ged(), "Mother", TRUE), 
#'              list("@I3@" = "1 NAME Mother /Bloggs/"))
get_records_by_pattern <- function(x, pattern, return_context = FALSE){
  
  rec_lines <- c(x@records@RAW@INDI, x@records@RAW@FAM, 
                 x@records@RAW@SOUR, x@records@RAW@REPO, x@records@RAW@SUBM,
                 x@records@RAW@OBJE, x@records@RAW@SNOTE)
  
  rec_lines_filt <- Filter(
    \(lines) any(grepl(pattern, parse_line_value(lines), ignore.case = TRUE)), 
    rec_lines
  )
  
  if(return_context)
    return(
      lapply(rec_lines_filt, 
             \(lines) grep(pattern, lines, ignore.case = TRUE, value = TRUE) 
      )
    )
  
  names(rec_lines_filt)
}

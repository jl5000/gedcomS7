
#' Add parent records for an individual
#' 
#' This function adds placeholder records for an individual's parents.
#' 
#' @details This function may also create a Family Group record and will 
#' not modify existing parents.
#'
#' @param x A gedcom object.
#' @param xref The xref of an Individual record.
#' @param inc_sex Whether to populate the sex of the parents. This will ensure
#' that there is one male and one female parent. Otherwise the sex will be
#' assigned as "U" (undetermined).
#'
#' @return A gedcom object with additional parent records.
#' @export
add_parents <- function(x, xref, inc_sex = TRUE, fath_name = NULL, moth_name = NULL){
  check_indi_rec(x, xref)
  
  moth_xref <- get_indi_mothers(x, xref, "BIRTH")
  fath_xref <- get_indi_fathers(x, xref, "BIRTH")
  
  if(length(moth_xref) > 0 && !is.null(moth_name))
    warning("Mother name not used as mother already exists")
  
  if(length(fath_xref) > 0 && !is.null(fath_name))
    warning("Father name not used as father already exists")
  
  if(length(fath_xref) > 0 && length(moth_xref) > 0)
    return(x)
    
  # Get family xref as child
  famc_xref <- get_fam_as_child(x, xref, "BIRTH")
  
  # Create new family record if necessary
  if(length(famc_xref) == 0){
    famc_xref <- x@next_xref[["fam"]]
    
    famc_rec <- class_record_fam(
      chil_xrefs = xref
    )
    
    x <- push_record(x, famc_rec)
  }

  # Add father
  if(length(fath_xref) == 0){
    
    par_rec <- class_record_indi(
      fam_links_spou = famc_xref
    )
    if(inc_sex) par_rec@sex <- "M"
    if(!is.null(fath_name)) par_rec@pers_names <- fath_name
    
    x <- push_record(x, par_rec)
  }
  
  # Add mother
  if(length(moth_xref) == 0){
    
    par_rec <- class_record_indi(
      fam_links_spou = famc_xref
    )
    if(inc_sex) par_rec@sex <- "F"
    if(!is.null(moth_name)) par_rec@pers_names <- moth_name
    
    x <- push_record(x, par_rec)
  }
  
  x
}

#' Create siblings for an Individual
#' 
#' @details This function may also create a Family Group record and will 
#' not modify existing siblings.
#'
#' @param x A gedcom object.
#' @param xref The xref of an Individual record.
#' @param sexes A character string giving the sexes of each sibling. For example,
#' "FFM" to add two sisters and one brother.
#'
#' @return A gedcom object with additional sibling records.
#' @export
add_siblings <- function(x, xref, sexes){
  check_indi_rec(x, xref)
  
  famc_xref <- get_fam_as_child(x, xref, "BIRTH")
  
  if(length(famc_xref) == 0){
    famc_xref <- x@next_xref[["fam"]]
    
    famc_rec <- class_record_fam(
      chil_xrefs = xref
    )
    
    x <- push_record(x, famc_rec)
  }
  
  sexes_vec <- unlist(strsplit(sexes, split = NULL))
  
  for(sx in sexes_vec){
    if(!sx %in% val_sexes()){
      warning("Skipping sibling with unknown sex: ", sx)
      next
    }
    
    sib_rec <- class_record_indi(
      sex = sx,
      fam_links_chil = famc_xref
    )
    
    x <- push_record(x, sib_rec)
  }
  
  x
}

#' Create multiple children for a Family Group
#'
#' @param x A gedcom object.
#' @param xref The xref of a Family Group record.
#' @param sexes A character string giving the sexes of each child. For example,
#' "FFM" to add two daughters and one son.
#'
#' @return A gedcom object with additional child records.
#' @export
add_children <- function(x, xref, sexes){
  check_fam_rec(x, xref)
  
  sexes_vec <- unlist(strsplit(sexes, split = NULL))
  
  for(sx in sexes_vec){
    if(!sx %in% val_sexes()){
      warning("Skipping child with unknown sex: ", sx)
      next
    }
    
    chil_rec <- class_record_indi(
      sex = sx,
      fam_links_chil = xref
    )

    x <- push_record(x, chil_rec)
  }
  
  x
}



#' Add a spouse for an individual
#' 
#' This creates a record for a spouse and their Family Group record.
#'
#' @param x A gedcom object.
#' @param xref The xref of an Individual record.
#' @param sex The sex of the spouse.
#'
#' @return A gedcom object with additional spouse and Family Group records.
#' @export
add_spouse <- function(x, xref, sex = "U", name = NULL){
  check_indi_rec(x, xref)
  
  spou_xref <- x@next_xref[["indi"]]
  
  spou_rec <- class_record_indi(
    sex = sex
  )
  if(!is.null(name)) spou_rec@pers_names <- name
  
  x <- push_record(x, spou_rec)
  
  if(sex == "M"){
    husb_xref = spou_xref
    wife_xref = xref
  } else if(sex == "F"){
    husb_xref = xref
    wife_xref = spou_xref
  } else {
    curr_sex <- find_ged_values(x@indi[[xref]], "SEX")
    if(length(curr_sex) == 1 && curr_sex == "F"){
      husb_xref = spou_xref
      wife_xref = xref
    } else {
      husb_xref = xref
      wife_xref = spou_xref
    }
  }
  
  fams_rec <- class_record_fam(
    husb_xref = husb_xref,
    wife_xref = wife_xref
  )
  
  push_record(x, fams_rec)
}


add_family_as_child <- function(x, xref){
  # add parents and siblings
}

add_family_as_spouse <- function(x, xref){
  # add spouse and children
}

#' Add ancestor records for an individual
#' 
#' This function adds placeholder Individual records for ancestors going back a specific
#' number of generations.
#' 
#' @details This function may also create Family records and will 
#' not modify existing ancestors.
#'
#' @param x A gedcom object.
#' @param xref The xref of an Individual record to add ancestors for.
#' @param num_gen The number of generations to create ancestors for.
#' @param inc_sex Whether to populate the sex of the ancestors. This will ensure
#' that there is one male and one female parent. Otherwise the sex will be
#' assigned as "U" (undetermined).
#'
#' @return A gedcom object with additional ancestor records.
#' @export
add_ancestors <- function(x, xref, num_gen, inc_sex = TRUE){
  check_indi_rec(x, xref)
  
  xrefs_par <- xref
  for(gen in seq_len(num_gen)){
    
    for(xref_par in xrefs_par){
      x <- add_parents(x, xref_par, inc_sex)
    }
    
    xrefs_par <- lapply(xrefs_par, get_indi_parents,
                        x = x,
                        pedigrees = "BIRTH") |>
      unlist()
  }
  
  x
}
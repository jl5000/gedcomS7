
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
add_parents <- function(x, xref, inc_sex = TRUE){
  if(!is_indi_xref(x, xref)) stop("The xref is not an Individual record.")
  
  pars_xref <- get_indi_parents(x, xref, birth_only = TRUE)
  
  if(length(pars_xref) == 2) return(x)
  
  # Get family group xref as child
  famc_xref <- get_famg_as_child(x, xref, birth_only = TRUE)
  if(length(famc_xref) > 1) stop("Individual already has more than one birth family.")
  
  # Create new family record if necessary
  if(length(famc_xref) == 0){
    famc_xref <- x@next_xref[["famg"]]
    
    famc_rec <- class_record_fam(
      chil_biol_xref = xref
    )
    
    x <- push_record(x, famc_rec)
  }

  if(length(pars_xref) == 1){
    # Add one parent
    par_rec <- class_record_indi(
      family_links = list(class_spouse_family_link(xref = famc_xref))
    )
    
    if(inc_sex){
      par_sex_cur <- find_ged_values(x@indi[[pars_xref]], "SEX")
      
      if(length(par_sex_cur) == 1){
        if(par_sex_cur == "F") par_rec@sex <- "M"
        if(par_sex_cur == "M") par_rec@sex <- "F"
      }
    }
    
    x <- push_record(x, par_rec)
    
  } else {
    # No parents - add them both
    husb_rec <- class_record_indi(
      family_links = list(class_spouse_family_link(xref = famc_xref))
    )
    wife_rec <- class_record_indi(
      family_links = list(class_spouse_family_link(xref = famc_xref))
    )
    
    if(inc_sex){
      husb_rec@sex <- "M"
      wife_rec@sex <- "F"
    }
    
    x <- push_record(x, husb_rec) |>
      push_record(wife_rec)
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
add_siblings <- function(x, xref, sexes = NULL){
  if(is.null(sexes)) return(x)
  if(!is_indi_xref(x, xref)) stop("The xref is not an Individual record.")
  
  famc_xref <- get_famg_as_child(x, xref, birth_only = TRUE)
  
  if(length(famc_xref) == 0){
    famc_xref <- x@next_xref[["famg"]]
    
    famc_rec <- class_record_fam(
      chil_biol_xref = xref
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
      family_links = list(class_child_family_link_biol(xref = famc_xref))
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
add_children <- function(x, xref, sexes = NULL){
  if(is.null(sexes)) return(x)
  if(!is_famg_xref(x, xref)) stop("The xref is not a Family Group record.")
  
  sexes_vec <- unlist(strsplit(sexes, split = NULL))
  
  for(sx in sexes_vec){
    if(!sx %in% val_sexes()){
      warning("Skipping child with unknown sex: ", sx)
      next
    }
    
    chil_rec <- class_record_indi(
      sex = sx,
      family_links = list(class_child_family_link_biol(xref = xref))
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
add_spouse <- function(x, xref, sex = "U"){
  if(!is_indi_xref(x, xref)) stop("The xref is not an Individual record.")
  
  spou_xref <- x@next_xref[["indi"]]
  
  spou_rec <- class_record_indi(
    sex = sex
  )
  
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


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
    
    famc_rec <- class_record_famg(
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
      sex = "M",
      family_links = list(class_spouse_family_link(xref = famc_xref))
    )
    wife_rec <- class_record_indi(
      sex = "F",
      family_links = list(class_spouse_family_link(xref = famc_xref))
    )
    
    x <- push_record(x, husb_rec) |>
      push_record(wife_rec)
  }
  
  x
}

add_children <- function(x, xref, sexes = NULL){
  if(!is_famg_xref(x, xref)) stop("The xref is not a Family Group record.")
  
}

add_siblings <- function(x, xref, sexes = NULL){
  if(!is_indi_xref(x, xref)) stop("The xref is not an Individual record.")
  
}

add_spouse <- function(x, xref, sex = "U"){
  if(!is_indi_xref(x, xref)) stop("The xref is not an Individual record.")
  
}
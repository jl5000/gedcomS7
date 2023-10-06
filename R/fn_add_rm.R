
#' Add parent records for an individual
#' 
#' This function adds records for an individual's parents.
#' 
#' @details This function may also create a Family record and will 
#' not modify existing parents.
#'
#' @param x A gedcom object.
#' @param xref The xref of an Individual record.
#' @param inc_sex Whether to populate the sex of the parents. This will ensure
#' that there is one male and one female parent. Otherwise the sex will be
#' assigned as "U" (undetermined).
#' @param fath_name,moth_name Optional names to give to the parents. 
#' Surnames must be enclosed in forward slashes.
#'
#' @return A gedcom object with additional parent records.
#' @export
add_parents <- function(x, xref, inc_sex = TRUE, fath_name = NULL, moth_name = NULL){
  check_indi_rec(x, xref)
  
  # Get first family xref as child - cannot assume there will be just one
  famc_xref <- get_fam_as_child(x, xref, "BIRTH")
  if(length(famc_xref) > 0) famc_xref <- famc_xref[1]
  
  # Create new family record if necessary
  if(length(famc_xref) == 0){
    famc_xref <- x@next_xref[["fam"]]
    
    famc_rec <- class_record_fam(
      chil_xrefs = xref
    )
    
    x <- push_record(x, famc_rec)
  }
  
  moth_xref <- find_ged_values(x@fam[[famc_xref]], "WIFE")
  fath_xref <- find_ged_values(x@fam[[famc_xref]], "HUSB")
  
  if(length(moth_xref) > 0 && !is.null(moth_name))
    warning("Mother name not used as mother already exists")
  
  if(length(fath_xref) > 0 && !is.null(fath_name))
    warning("Father name not used as father already exists")
  
  if(length(fath_xref) > 0 && length(moth_xref) > 0)
    return(x)
    
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
#' @details This function may also create a Family record and will 
#' not modify existing siblings.
#'
#' @param x A gedcom object.
#' @param xref The xref of an Individual record.
#' @param sexes A character string giving the sexes of each sibling. For example,
#' "FFM" to add two sisters and one brother.
#' @param sib_names A character vector of sibling's names. If provided, it must be
#' the same length as the number of sexes. If you don't want to provide a name for a
#' sibling, set the name to "". Surnames must be enclosed in forward slashes.
#'
#' @return A gedcom object with additional sibling records.
#' @export
add_siblings <- function(x, xref, sexes, sib_names = NULL){
  check_indi_rec(x, xref)
  if(!is.null(sib_names) && nchar(sexes) != length(sib_names))
    stop("If sibling names are given, the length of sib_names must be equal to the number of sexes given.")
  
  famc_xref <- get_fam_as_child(x, xref, "BIRTH")
  
  if(length(famc_xref) == 0){
    famc_xref <- x@next_xref[["fam"]]
    
    famc_rec <- class_record_fam(
      chil_xrefs = xref
    )
    
    x <- push_record(x, famc_rec)
  }
  
  sexes_vec <- unlist(strsplit(sexes, split = NULL))
  
  for(i in seq_along(sexes_vec)){
    if(!sexes_vec[i] %in% val_sexes()){
      warning("Skipping sibling with unknown sex: ", sexes_vec[i])
      next
    }
    
    sib_rec <- class_record_indi(
      sex = sexes_vec[i],
      fam_links_chil = famc_xref
    )
    if(!is.null(sib_names) && sib_names[i] != "") sib_rec@pers_names <- sib_names[i]
    
    x <- push_record(x, sib_rec)
  }
  
  x
}

#' Create multiple children for a Family
#'
#' @param x A gedcom object.
#' @param xref The xref of a Family record.
#' @param sexes A character string giving the sexes of each child. For example,
#' "FFM" to add two daughters and one son.
#' @param chil_names A character vector of children's names. If provided, it must be
#' the same length as the number of sexes. If you don't want to provide a name for a
#' child, set the name to "". Surnames must be enclosed in forward slashes.
#'
#' @return A gedcom object with additional child records.
#' @export
add_children <- function(x, xref, sexes, chil_names = NULL){
  check_fam_rec(x, xref)
  if(!is.null(chil_names) && nchar(sexes) != length(chil_names))
    stop("If child names are given, the length of chil_names must be equal to the number of sexes given.")
  
  sexes_vec <- unlist(strsplit(sexes, split = NULL))
  
  for(i in seq_along(sexes_vec)){
    if(!sexes_vec[i] %in% val_sexes()){
      warning("Skipping child with unknown sex: ", sexes_vec[i])
      next
    }
    
    chil_rec <- class_record_indi(
      sex = sexes_vec[i],
      fam_links_chil = xref
    )
    if(!is.null(chil_names) && chil_names[i] != "") chil_rec@pers_names <- chil_names[i]

    x <- push_record(x, chil_rec)
  }
  
  x
}



#' Add a spouse for an individual
#' 
#' This creates a record for a spouse and their Family record.
#'
#' @param x A gedcom object.
#' @param xref The xref of an Individual record.
#' @param sex The sex of the spouse.
#' @param spou_name Optional name to give to the spouse.
#' Surnames must be enclosed in forward slashes.
#'
#' @return A gedcom object with additional spouse and Family Group records.
#' @export
add_spouse <- function(x, xref, sex = "U", spou_name = NULL){
  check_indi_rec(x, xref)
  
  spou_xref <- x@next_xref[["indi"]]
  
  spou_rec <- class_record_indi(
    sex = sex
  )
  if(!is.null(spou_name)) spou_rec@pers_names <- spou_name
  
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





#' Remove records from a GEDCOM object
#'
#' @param x A gedcom object.
#' @param xrefs A character vector of xrefs to remove.
#'
#' @return The gedcom object with the records removed. Pointers to the record will
#' be replaced with @VOID@.
#' @export
rm_records <- function(x, xrefs){
  xrefs <- unique(xrefs)
  for(xref in xrefs){
    for(rec_type in val_record_types()){
      
      # Delete the record (if it is this type of record)
      S7::prop(x, rec_type)[[xref]] <- NULL
      
      # Delete the pointers to it
      S7::prop(x, rec_type) <- lapply(S7::prop(x, rec_type), 
                                      \(lines) void_xref_ptrs(lines, xref))
    }
  }
  x
}


void_xref_ptrs <- function(lines, xref){
  rows <- extract_ged_value(lines) == xref
  if(sum(rows) == 0) return(lines)
  lines[rows] <- sub(paste0(xref, "$"), "@VOID@", lines[rows])
  lines
}
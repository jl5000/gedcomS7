
chk_input_size <- function(input, name, min_dim, max_dim, min_char = NULL, max_char = NULL) {
  
  if (length(input) < min_dim) 
    return(sprintf("%s has too few dimensions. The minimum is %s.", name, min_dim))
  
  if (length(input) > max_dim) 
    return(sprintf("%s has too many dimensions. The maximum is %s.", name, max_dim))
  
  if (length(input) > 0) {
    
    if (!is.null(min_char) && min(nchar(input)) < min_char)
      return(sprintf("%s has too few characters. The minimum is %s.", name, min_char))
    
    if (!is.null(max_char) && max(nchar(input)) > max_char)
      return(sprintf("%s has too many characters. The maximum is %s.", name, max_char))
    
  }
  
  NULL
}


chk_input_pattern <- function(input, name, pattern) {
  if (length(input) > 0 && is.character(input)) {
    for (i in input) {
      if (!grepl(pattern, i))
        return(sprintf("%s is in an invalid format.", name))
    }
  }
  NULL
}


chk_input_choice <- function(input, name, choices) {
  if (length(input) > 0 && is.character(input)){
    for (i in input) {
      if (!i %in% choices)
        return(sprintf("%s has an invalid value:\n  The valid values are: %s", 
                                 name, paste(choices, collapse = ", ")))
    }
  } 
  NULL
}


chk_input_R7classes <- function(inputs, name, target_class){
  target_class_name <- target_class@name
  for(inp in inputs){
    if(!R7::R7_inherits(inp, target_class))
      return(sprintf("%s contains an invalid object not of %s.", 
                     name, target_class_name))
  }
  NULL
}

chk_xref_pointers_valid <- function(x){
  xrefs <- unname(unlist(x@xrefs))
  if(sum(duplicated(xrefs)) > 0)
    return(paste0("Duplicate record xrefs found: ", 
                  paste(xrefs[which(duplicated(xrefs))], collapse = ", ")))
  
  if(length(x@xref_subm) > 0 && x@subm@xref != x@xref_subm)
    return("Inconsistent submitter record xref.")
  
  xdf <- x@as_df
  
  husbands <- dplyr::filter(xdf, tag == "HUSB") |>
    dplyr::pull(value)
  fictional_husbands <- setdiff(husbands, x@xrefs["indi"])
  if(length(fictional_husbands) > 0)
    return(paste("Husband not found:", fictional_husbands))
  
  
  
  # xrefs in famg record and corresponding Famc/fams structures
  # for(famg in x@famg){
  #   famg_xref <- famg@xref
  #   for(husb in famg@husb_xref){
  #     if(!husb %in% x@xrefs[["indi"]])
  #       return(sprintf("Husband {husb} does not exist."))
  #     for(link in x@indi@links){
  #       
  #     }
  #   }
  #   for(wife in famg@wife_xref){
  #     if(!wife %in% x@xrefs[["indi"]])
  #       return(sprintf("Wife {wife} does not exist."))
  #   }
  #   for(chil in famg@chil_xref){
  #     if(!chil %in% x@xrefs[["indi"]])
  #       return(sprintf("Child {chil} does not exist."))
  #   }
  # }
  # associations
  # media links
  # notes links
  # citation links
  # source repo link
  
  NULL
}
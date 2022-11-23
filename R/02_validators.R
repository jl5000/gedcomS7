
chk_input_size <- function(input, name, min_dim, max_dim, min_char = NULL, max_char = NULL) {
  
  if (length(input) < min_dim) 
    return(stringr::str_glue("{name} has too few dimensions. The minimum is {max_dim}."))
  
  if (length(input) > max_dim) 
    return(stringr::str_glue("{name} has too many dimensions. The maximum is {max_dim}."))
  
  if (length(input) > 0) {
    
    if (!is.null(min_char) && min(nchar(input)) < min_char)
      return(stringr::str_glue("{name} has too few characters. The minimum is {min_char}."))
    
    if (!is.null(max_char) && max(nchar(input)) > max_char)
      return(stringr::str_glue("{name} has too many characters. The maximum is {max_char}."))
    
  }
  
  NULL
}


chk_input_pattern <- function(input, name, pattern) {
  if (length(input) > 0 && is.character(input)) {
    for (i in input) {
      if (!grepl(pattern, i))
        return(stringr::str_glue("{name} is in an invalid format."))
    }
  }
  NULL
}


chk_input_choice <- function(input, name, choices) {
  if (length(input) > 0 && is.character(input)){
    for (i in input) {
      if (!i %in% choices)
        return(stringr::str_glue("{name} has an invalid value:\n  The valid values are: ", 
                                 paste(choices, collapse = ", ")))
    }
  } 
  NULL
}


chk_input_R7classes <- function(inputs, name, target_class){
  target_class_name <- target_class@name
  for(inp in inputs){
    if(!R7::R7_inherits(inp, target_class))
      return(stringr::str_glue("{name} contains an invalid object not of {target_class_name}."))
  }
  NULL
}


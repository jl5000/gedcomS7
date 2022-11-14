
chk_input_size <- function(input, name, max_dim, min_char = NULL, max_char = NULL) {
  
  if (length(input) > max_dim) 
    return(stringr::str_glue("{name} has too many dimensions. The limit is {max_dim}."))
  
  if (length(input) > 0) {
    
    if (!is.null(min_char) && min(nchar(input)) < min_char)
      return(stringr::str_glue("{name} has too few characters. The minimum is {min_char}."))
    
    if (!is.null(max_char) && max(nchar(input)) > max_char)
      return(stringr::str_glue("{name} has too many characters. The maximum is {max_char}."))
    
  }
  
  NULL
}


chk_input_pattern <- function(input, name, pattern) {
  if (length(input) > 0) {
    for (i in input) {
      if (!grepl(pattern, i))
        return(stringr::str_glue("{name} is in an invalid format."))
    }
  }
  NULL
}


chk_input_choice <- function(input, name, choices) {
  if (length(input) == 1 && !input %in% choices) 
    return(stringr::str_glue("{name} has an invalid value:\n  The valid values are: ", 
                 paste(choices, collapse = ", ")))
  NULL
}



anchor_it <- function(reg) {
  paste0("^", reg, "$")
}
reg_xref <- function(only = TRUE) {
  #p31
  reg <- "@[a-zA-Z0-9]{1,20}@"
  if(only) reg <- anchor_it(reg)
  reg
}

reg_age_at_event <- function() {
  paste0("^(?:[<>] )?",
         c("\\d{1,3}y \\d{1,2}m \\d{1,3}d$",
           "\\d{1,3}y \\d{1,2}m$",
           "\\d{1,3}y \\d{1,3}d$",
           "\\d{1,2}m \\d{1,3}d$",
           "\\d{1,3}y$",
           "\\d{1,2}m$",
           "\\d{1,3}d$")) |> 
    paste(collapse = "|")
  
}

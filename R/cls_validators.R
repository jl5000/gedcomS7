#' @include utils_at.R utils_lookups.R utils_regex.R
NULL

chk_input_size <- function(input, 
                           name, 
                           min_dim = NULL, 
                           max_dim = NULL, 
                           min_char = NULL, 
                           max_char = NULL) {
  
  if (!is.null(min_dim) && length(input) < min_dim) 
    return(sprintf("%s has too few dimensions. The minimum is %s.", name, min_dim))
  
  if (!is.null(max_dim) && length(input) > max_dim) 
    return(sprintf("%s has too many dimensions. The maximum is %s.", name, max_dim))
  
  if (length(input) > 0 && !is.null(min_char) && is.character(input) && min(nchar(input)) < min_char) 
    return(sprintf("%s has too few characters. The minimum is %s.", name, min_char))
  
  if (length(input) > 0 && !is.null(max_char) && is.character(input) && max(nchar(input)) > max_char) 
    return(sprintf("%s has too many characters. The maximum is %s.", name, max_char))
  
  if (length(input) > 0 && !is.null(min_char) && is.numeric(input) && min(input) < min_char) 
    return(sprintf("%s has a value which is too low. The minimum is %s.", name, min_char))
  
  if (length(input) > 0 && !is.null(max_char) && is.numeric(input) && max(input) > max_char) 
    return(sprintf("%s has a value which is too high. The maximum is %s.", name, max_char))
    
}


chk_input_pattern <- function(input, name, pattern) {
  if (length(input) > 0 && is.character(input)) {
    for (i in input) {
      if (!grepl(pattern, i))
        return(sprintf("%s is in an invalid format.", name))
    }
  }
}


chk_input_choice <- function(input, name, choices) {
  if (length(input) > 0 && is.character(input)){
    for (i in input) {
      if (!i %in% choices)
        return(sprintf("%s has an invalid value:\n  The valid values are: %s", 
                                 name, toString(choices)))
    }
  } 
}


chk_input_S7classes <- function(inputs, name, target_class, backup_pattern = NULL){
  
  for(inp in inputs){
    if(is.character(inp) && !is.null(backup_pattern)){
      if(!grepl(backup_pattern, inp))
        return(sprintf("%s is in an invalid format.", name))
    } else {
      if(!S7::S7_inherits(inp, target_class))
        return(sprintf("%s contains an invalid object not of %s.", 
                       name, target_class@name))
    }
    
  }
}

chk_input_date <- function(year, month, day, bce = FALSE){
  if (length(year) == 0)
    return("Year must be defined")
  
  if(bce){
    if(length(year) == 0 || length(month) + length(day) > 0)
      return("BCE date must contain year only")
  }
  
  if (length(month) < length(day))
    return("Day is defined without a month")
  
  if(length(year) == 0) year <- 2000
  if(length(month) == 0) month <- 10
  if(length(day) == 0) day <- 10
  if(nchar(day) == 1) day <- paste0(0, day)
  if(nchar(month) == 1) month <- paste0(0, month)
  d <- try(as.Date(paste(day, month, year), format = "%d %m %Y"))
  if("try-error" %in% class(d) || is.na(d)) {
    return("Invalid date")
  }
}

chk_input_dates <- function(start_date, end_date){

  if(length(start_date) + length(end_date) < 2) return()
  
  start_val <- datetime_to_val(start_date)
  end_val <- datetime_to_val(end_date)
  
  start_year <- grep("^\\d{1,2}\\b", start_val, value = TRUE)
  end_year <- grep("^\\d{1,2}\\b", end_val, value = TRUE)
  
  
  day <- grep("^\\d{1,2}\\b", start_val, value = TRUE)
  month <- which(grep("", start_val, value = TRUE) == toupper(month.abb))
  year <- grep("^\\d{1,2}\\b", start_val, value = TRUE)
  start <- paste(sprintf("%02d", day), 
                 sprintf("%02d", month), 
                 year) |> 
    as.Date(format = "%d %m %Y")
  
  day <- 1
  month <- 1
  year <- 1
  end <- paste(sprintf("%02d", day), 
               sprintf("%02d", month), 
               year) |> 
    as.Date(format = "%d %m %Y")
  
  if(start == end)
    return("Start date is the same as end date")
  
  if(start > end)
    return("Start date comes after end date")
    
}

chk_whole_number <- function(input, name){
  if(length(input) == 1 && floor(input) != input)
    return(sprintf("%s must be a whole number", name))
}

chk_input_parents <- function(input, name, parent, parent_name){
  if(length(parent) == 0 && length(input) > 0)
    return(sprintf("%s requires a %s", name, parent_name))
}
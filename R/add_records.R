
add_indi <- function(x, 
                     sex = "U", 
                     user_reference_numbers = character(), 
                     notes = character(), 
                     xref = NULL){
  
  if(is.null(xref)) xref <- unname(x@next_xref["indi"])
  x@indi[[xref]] <- class_record_indi(xref = xref, 
                                      sex = sex,
                                      user_reference_numbers = user_reference_numbers,
                                      notes = notes)
  sex <- ifelse(length(sex) == 0, "unknown sex", paste("sex", sex))
  message(sprintf("Individual of %s added with xref %s.", sex, xref))
  x
}

add_famg <- function(x, 
                     husb_xref = character(), 
                     wife_xref = character(), 
                     chil_xref = character(),
                     user_reference_numbers = character(), 
                     notes = character(), 
                     xref = NULL){
  
  if(is.null(xref)) xref <- unname(x@next_xref["famg"])
  x@famg[[xref]] <- class_record_famg(xref = xref,
                                      husb_xref = husb_xref, 
                                      wife_xref = wife_xref, 
                                      chil_xref = chil_xref,
                                      user_reference_numbers = user_reference_numbers,
                                      notes = notes)
  message(sprintf("Family group added with xref %s.", xref))
  x
}

add_sour <- function(x, 
                     title = character(), 
                     user_reference_numbers = character(), 
                     notes = character(), 
                     xref = NULL){
  
  if(is.null(xref)) xref <- unname(x@next_xref["sour"])
  x@sour[[xref]] <- class_record_sour(xref = xref, 
                                      full_title = title,
                                      user_reference_numbers = user_reference_numbers,
                                      notes = notes)
  title <- ifelse(length(title) == 0, "unknown title", paste("title", paste0("'", title, "'")))
  message(sprintf("Source with %s added with xref %s.", title, xref))
  x
}

add_repo <- function(x, 
                     name, 
                     user_reference_numbers = character(), 
                     notes = character(), 
                     xref = NULL){
  
  if(is.null(xref)) xref <- unname(x@next_xref["repo"])
  x@repo[[xref]] <- class_record_repo(xref = xref, 
                                      name = name,
                                      user_reference_numbers = user_reference_numbers,
                                      notes = notes)
  message(sprintf("Repository named %s added with xref %s.", name, xref))
  x
}

add_media <- function(x, 
                      file_ref, 
                      format, 
                      user_reference_numbers = character(), 
                      notes = character(), 
                      xref = NULL){
  
  if(is.null(xref)) xref <- unname(x@next_xref["media"])
  x@media[[xref]] <- class_record_media(xref = xref, 
                                        file_ref = file_ref, 
                                        format = format,
                                        user_reference_numbers = user_reference_numbers,
                                        notes = notes)
  message(sprintf("Multimedia %s with ref %s added with xref %s.", format, file_ref, xref))
  x
}

add_note <- function(x, 
                     text, 
                     user_reference_numbers = character(), 
                     xref = NULL){
  
  if(is.null(xref)) xref <- unname(x@next_xref["note"])
  x@note[[xref]] <- class_record_note(xref = xref, 
                                      text = text,
                                      user_reference_numbers = user_reference_numbers)
  start <- substr(text, 1, 20)
  message(sprintf("Note beginning '%s...' added with xref %s.", start, xref))
  x
}

rm_indi <- function(x, xref){
  chk_valid_xref(x, xref, "indi")
  x@indi[[xref]] <- NULL
  x  
}
rm_famg <- function(x, xref){
  chk_valid_xref(x, xref, "famg")
  x@famg[[xref]] <- NULL
  x  
}
rm_sour <- function(x, xref){
  chk_valid_xref(x, xref, "sour")
  x@sour[[xref]] <- NULL
  x  
}
rm_repo <- function(x, xref){
  chk_valid_xref(x, xref, "repo")
  x@repo[[xref]] <- NULL
  x  
}
rm_media <- function(x, xref){
  chk_valid_xref(x, xref, "media")
  x@media[[xref]] <- NULL
  x  
}
rm_note <- function(x, xref){
  chk_valid_xref(x, xref, "note")
  x@note[[xref]] <- NULL
  x  
}
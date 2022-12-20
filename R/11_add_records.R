
add_indi <- function(x, sex = "U", user_reference_numbers = character(), notes = character()){
  xref <- unname(x@next_xref["indi"])
  x@indi[[length(x@indi) + 1]] <- class_record_indi(xref = xref, 
                                                    sex = sex,
                                                    user_reference_numbers = user_reference_numbers,
                                                    notes = notes)
  sex <- ifelse(length(sex) == 0, "unknown sex", paste("sex", sex))
  message(sprintf("Individual of %s added with xref %s.", sex, xref))
  x
}

add_famg <- function(x, husb_xref = character(), wife_xref = character(), chil_xref = character(),
                     user_reference_numbers = character(), notes = character()){
  xref <- unname(x@next_xref["famg"])
  x@famg[[length(x@famg) + 1]] <- class_record_famg(xref = xref,
                                                    user_reference_numbers = user_reference_numbers,
                                                    notes = notes)
  # Assign properties here to trigger the setter which does not happen on instantiation
  R7::props(x@famg[[xref]]) <- list(husb_xref = husb_xref, wife_xref = wife_xref, chil_xref = chil_xref)
  message(sprintf("Family group added with xref %s.", xref))
  x
}

add_sour <- function(x, title = character(), user_reference_numbers = character(), notes = character()){
  xref <- unname(x@next_xref["sour"])
  x@sour[[length(x@sour) + 1]] <- class_record_sour(xref = xref, 
                                                    full_title = title,
                                                    user_reference_numbers = user_reference_numbers,
                                                    notes = notes)
  title <- ifelse(length(title) == 0, "unknown title", paste("title", paste0("'", title, "'")))
  message(sprintf("Source with %s added with xref %s.", title, xref))
  x
}

add_repo <- function(x, name, user_reference_numbers = character(), notes = character()){
  xref <- unname(x@next_xref["repo"])
  x@repo[[length(x@repo) + 1]] <- class_record_repo(xref = xref, 
                                                    name = name,
                                                    user_reference_numbers = user_reference_numbers,
                                                    notes = notes)
  message(sprintf("Repository named %s added with xref %s.", name, xref))
  x
}

add_media <- function(x, file_ref, format, user_reference_numbers = character(), notes = character()){
  xref <- unname(x@next_xref["media"])
  x@media[[length(x@media) + 1]] <- class_record_media(xref = xref, 
                                                       file_ref = file_ref, 
                                                       format = format,
                                                       user_reference_numbers = user_reference_numbers,
                                                       notes = notes)
  message(sprintf("Multimedia %s with ref %s added with xref %s.", format, file_ref, xref))
  x
}

add_note <- function(x, text, user_reference_numbers = character()){
  xref <- unname(x@next_xref["note"])
  x@note[[length(x@note) + 1]] <- class_record_note(xref = xref, 
                                                    text = text,
                                                    user_reference_numbers = user_reference_numbers)
  start <- substr(text, 1, 20)
  message(sprintf("Note beginning '%s...' added with xref %s.", start, xref))
  x
}

rm_indi <- function(x, xref){
  if(xref %notin% x@xrefs[["indi"]])
    warning(sprintf("Record with xref %s not found.", xref))
  x@indi[[xref]] <- NULL
  x  
}
rm_famg <- function(x, xref){
  if(xref %notin% x@xrefs[["famg"]])
    warning(sprintf("Record with xref %s not found.", xref))
  x@famg[[xref]] <- NULL
  x  
}
rm_sour <- function(x, xref){
  if(xref %notin% x@xrefs[["sour"]])
    warning(sprintf("Record with xref %s not found.", xref))
  x@sour[[xref]] <- NULL
  x  
}
rm_repo <- function(x, xref){
  if(xref %notin% x@xrefs[["repo"]])
    warning(sprintf("Record with xref %s not found.", xref))
  x@repo[[xref]] <- NULL
  x  
}
rm_media <- function(x, xref){
  if(xref %notin% x@xrefs[["media"]])
    warning(sprintf("Record with xref %s not found.", xref))
  x@media[[xref]] <- NULL
  x  
}
rm_note <- function(x, xref){
  if(xref %notin% x@xrefs[["note"]])
    warning(sprintf("Record with xref %s not found.", xref))
  x@note[[xref]] <- NULL
  x  
}

add_indi <- function(x, sex = "U"){
  xref <- unname(x@next_xref["indi"])
  x@indi[[length(x@indi) + 1]] <- class_record_indi(xref = xref, sex = sex)
  sex <- ifelse(length(sex) == 0, "unknown sex", paste("sex", sex))
  message(stringr::str_glue("Individual of {sex} added with xref {xref}."))
  x
}

add_famg <- function(x, husb_xref = character(), wife_xref = character(), chil_xref = character()){
  xref <- unname(x@next_xref["famg"])
  x@famg[[length(x@famg) + 1]] <- class_record_famg(xref = xref)
  # Assign properties here to trigger the setter which does not happen on instantiation
  R7::props(x@famg[[xref]]) <- list(husb_xref = husb_xref, wife_xref = wife_xref, chil_xref = chil_xref)
  message(stringr::str_glue("Family group added with xref {xref}."))
  x
}

add_sour <- function(x, title = character()){
  xref <- unname(x@next_xref["sour"])
  x@sour[[length(x@sour) + 1]] <- class_record_sour(xref = xref, full_title = title)
  title <- ifelse(length(title) == 0, "unknown title", paste("title", paste0("'", title, "'")))
  message(stringr::str_glue("Source with {title} added with xref {xref}."))
  x
}

add_repo <- function(x, name){
  xref <- unname(x@next_xref["repo"])
  x@repo[[length(x@repo) + 1]] <- class_record_repo(xref = xref, name = name)
  message(stringr::str_glue("Repository named {name} added with xref {xref}."))
  x
}

add_media <- function(x, file_ref, format){
  xref <- unname(x@next_xref["media"])
  x@media[[length(x@media) + 1]] <- class_record_media(xref = xref, file_ref = file_ref, format = format)
  message(stringr::str_glue("Multimedia {format} with ref {file_ref} added with xref {xref}."))
  x
}

add_note <- function(x, text){
  xref <- unname(x@next_xref["note"])
  x@note[[length(x@note) + 1]] <- class_record_note(xref = xref, text = text)
  start <- substr(text, 1, 20)
  message(stringr::str_glue("Note beginning '{start}...' added with xref {xref}."))
  x
}
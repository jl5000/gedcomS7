
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
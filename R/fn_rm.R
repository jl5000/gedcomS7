


rm_records <- function(x, xrefs){
  xrefs <- unique(xrefs)
  for(xref in xrefs){
    if(is_indi_xref(x, xref)){
      x <- rm_indi(x, xref)
    } else if(is_famg_xref(x, xref)){
      x <- rm_famg(x, xref)
    } else if(is_sour_xref(x, xref)){
      x <- rm_sour(x, xref)
    } else if(is_repo_xref(x, xref)){
      x <- rm_repo(x, xref)
    } else if(is_media_xref(x, xref)){
      x <- rm_media(x, xref)
    } else if(is_note_xref(x, xref)){
      x <- rm_note(x, xref)
    } else {
      warning("There is no record with xref ", xref)
    }
  }
  x
}

# TODO: remove other links to these records
rm_indi <- function(x, xref){
  x@indi[[xref]] <- NULL
  # Remove associations
  # Remove family membership
  x  
}
rm_famg <- function(x, xref){
  x@famg[[xref]] <- NULL
  # Remove family links
  # Also in birth/chris/adop events
  x  
}
rm_sour <- function(x, xref){
  x@sour[[xref]] <- NULL
  # Remove citations
  x  
}
rm_repo <- function(x, xref){
  x@repo[[xref]] <- NULL
  # Remove repo citations
  x  
}
rm_media <- function(x, xref){
  x@media[[xref]] <- NULL
  # Remove media links
  x  
}
rm_note <- function(x, xref){
  x@note[[xref]] <- NULL
  # Remove note links
  x  
}

add_notes <- function(x, texts = character(), xrefs = character()){
  for(text in texts){
    x@notes[[length(x@notes) + 1]] <- class_note(text = text)
  }
  for(xref in xrefs){
    x@notes[[length(x@notes) + 1]] <- class_note(xref = xref)
  }
  x
}

add_media_links <- function(x, xrefs){
  for(xref in xrefs){
    x@media_links[[length(x@media_links) + 1]] <- class_media_link(xref = xref)
  }
  x
}

add_citation <- function(x, citation){
  x@citations[[length(x@citations) + 1]] <- citation
  x
}

add_indi_fact <- function(x, fact){
  x@facts[[length(x@facts) + 1]] <- fact
  x
}

add_famg_event <- function(x, event){
  x@events[[length(x@events) + 1]] <- events
  x
}
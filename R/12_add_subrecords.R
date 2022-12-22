

add_indi_family_link <- function(indi, xref_famg, as_child = TRUE, pedigree = "birth"){
  if(as_child){
    indi@family_links[[length(indi@family_links) + 1]] <- class_child_to_family_link(xref = xref_famg,
                                                                                     pedigree = pedigree)
  } else {
    indi@family_links[[length(indi@family_links) + 1]] <- class_spouse_to_family_link(xref = xref_famg)
  }
  indi
}

add_indi_fact <- function(x, fact){
  x@facts[[length(x@facts) + 1]] <- fact
  x
}

add_famg_event <- function(x, event){
  x@events[[length(x@events) + 1]] <- events
  x
}
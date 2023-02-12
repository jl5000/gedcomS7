


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
  for(i in seq_along(x@indi)){
    asso_rows <- grep(sprintf("^1 ASSO %s$", xref), x@indi[[i]])
    while(length(asso_rows) > 0){
      x@indi[[i]] <- delete_ged_section(x@indi[[i]], asso_rows[1])
      asso_rows <- grep(sprintf("^1 ASSO %s$", xref), x@indi[[i]])
    }
  }
  # Remove family membership
  for(i in seq_along(x@famg)){
    vals <- extract_ged_value(x@famg[[i]])
    memb_rows <- which(vals == xref)
    if(memb_rows > 0)
      x@famg[[i]] <- x@famg[[i]][-memb_rows]
  }
  
  x  
}

rm_famg <- function(x, xref){
  x@famg[[xref]] <- NULL
  # Remove family links
  # Also in birth/chris/adop events
  for(i in seq_along(x@indi)){
    link_rows <- grep(sprintf("^(1|2) (FAMS|FAMC) %s$", xref), x@indi[[i]])
    while(length(link_rows) > 0){
      x@indi[[i]] <- delete_ged_section(x@indi[[i]], link_rows[1])
      link_rows <- grep(sprintf("^(1|2) (FAMS|FAMC) %s$", xref), x@indi[[i]])
    }
  }
  
  x  
}

rm_sour <- function(x, xref){
  x@sour[[xref]] <- NULL
  # Remove citations
  rec_types <- c("indi", "famg", "media", "note")
  for(rec_type in rec_types){
    for(i in seq_along(R7::prop(x, rec_type))){
      cit_rows <- grep(sprintf("^[1-6] SOUR %s$", xref), 
                       R7::prop(x, rec_type)[[i]])
      while(length(cit_rows) > 0){
        R7::prop(x, rec_type)[[i]] <- delete_ged_section(R7::prop(x, rec_type)[[i]], 
                                                         cit_rows[1])
        cit_rows <- grep(sprintf("^[1-6] SOUR %s$", xref), 
                         R7::prop(x, rec_type)[[i]])
      }
    }
  }
  
  x  
}

rm_repo <- function(x, xref){
  x@repo[[xref]] <- NULL
  # Remove repo citations
  for(i in seq_along(x@sour)){
    cit_rows <- grep(sprintf("^1 REPO %s$", xref), x@sour[[i]])
    while(length(cit_rows) > 0){
      x@sour[[i]] <- delete_ged_section(x@sour[[i]], cit_rows[1])
      cit_rows <- grep(sprintf("^1 REPO %s$", xref), x@sour[[i]])
    }
  }
  
  x  
}

rm_media <- function(x, xref){
  x@media[[xref]] <- NULL
  # Remove media links (inc. submitter)
  x@subm@media_links <- x@subm@media_links[x@subm@media_links != xref]
  
  rec_types <- c("indi", "famg", "media", "note", "sour")
  for(rec_type in rec_types){
    for(i in seq_along(R7::prop(x, rec_type))){
      vals <- extract_ged_value(R7::prop(x, rec_type)[[i]])
      link_rows <- which(vals == xref)
      if(link_rows > 0)
        R7::prop(x, rec_type)[[i]] <- R7::prop(x, rec_type)[[i]][-link_rows]
    }
  }
  x  
}

rm_note <- function(x, xref){
  x@note[[xref]] <- NULL
  # Remove note links (inc Submitter)
  x@subm@note_links <- x@subm@note_links[x@subm@note_links != xref]
  if(!is.null(x@subm@last_updated))
    x@subm@last_updated@note_links <- x@subm@last_updated@note_links[x@subm@last_updated@note_links != xref]
  
  rec_types <- c("indi", "famg", "media", "note", "sour")
  for(rec_type in rec_types){
    for(i in seq_along(R7::prop(x, rec_type))){
      vals <- extract_ged_value(R7::prop(x, rec_type)[[i]])
      note_rows <- which(vals == xref)
      if(note_rows > 0)
        R7::prop(x, rec_type)[[i]] <- R7::prop(x, rec_type)[[i]][-note_rows]
    }
  }
  x  
}

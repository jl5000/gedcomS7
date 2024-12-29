
#' Pull a record from a GEDCOM object for editing
#' 
#' @details The record is not removed from the gedcom object, rather a copy is taken.
#'
#' @param x A gedcom object.
#' @param xref The xref of the record to pull.
#'
#' @return An S7 object representing the record.
#' @export
pull_record <- function(x, xref){
  
  if(!xref %in% unlist(x@c_xrefs))
    stop("The xref is not in the GEDCOM object")
  
  rec_lines <- c(x@indi, x@fam, x@sour, x@repo,
                 x@media, x@note, x@subm)[[xref]]
  
  rec_type <- parse_line_tag(rec_lines[1])
  if(!rec_type %in% c("INDI","FAM","SOUR","REPO","SNOTE","OBJE","SUBM"))
    stop("Record type not recognised: ", rec_type)
  
  rec <- switch(rec_type,
                INDI = parse_record_indi(rec_lines),
                FAM = parse_record_fam(rec_lines),
                SOUR = parse_record_sour(rec_lines),
                REPO = parse_record_repo(rec_lines),
                OBJE = parse_record_media(rec_lines),
                SNOTE = parse_record_note(rec_lines),
                SUBM = parse_record_subm(rec_lines)
  )
  
  check_unparsed(rec_lines, rec)

  if(rec@locked){
    warning("The record is locked. Ensure you have the record owner's permission before editing it and pushing it back to the GEDCOM object.") 
  } else {
    if(is_indi_rec(rec) || is_fam_rec(rec)){
      # Check facts
      locked_facts <- vapply(rec@facts, \(fct) fct@locked, FUN.VALUE = logical(1))
      
      if(sum(locked_facts) > 0)
        warning(paste("The following facts are locked. Ensure you have the record owner's permission before editing it and pushing it back to the GEDCOM object:",
                      toString(which(locked_facts)))) 
    }
  }

  rec
  
}

#' Push an edited record back into a GEDCOM object
#' 
#' @details 
#' The function will automatically keep family links for individuals updated.
#' It will also update the record last_updated if update_change_dates in the
#' gedcom object is set to TRUE.
#'
#' @param gedcom An object representing the GEDCOM file.
#' @param record An object representing the record to place back into the
#' GEDCOM object.
#'
#' @return An updated GEDCOM object.
#' @export
push_record <- function(gedcom, record){
  
  if(gedcom@update_change_dates){
     record@updated <- ChangeDate()
  }
  
  if(gedcom@add_creation_dates){
    if(length(record@created) == 0 && record@xref == "@GEDCOMS7_ORPHAN@"){
      record@created <- CreationDate()
    }
  }
  
  rec_type <- get_record_type(record)
  
  if(record@xref == "@GEDCOMS7_ORPHAN@"){
    record@xref <- unname(gedcom@c_next_xref[rec_type])
    message("New ", names(which(val_record_types() == rec_type)), " record added with xref ", record@xref)
  }
  
  # Don't do this yet
  #if(rec_type %in% c("indi","fam")) record <- order_facts(record)
    
  S7::prop(gedcom, rec_type)[[record@xref]] <- record@c_as_ged
  
  if(rec_type == "indi"){
    gedcom <- refresh_indi_links(gedcom, record)
  } else if(rec_type == "fam"){
    gedcom <- refresh_fam_links(gedcom, record)
  }
  
  gedcom
}


refresh_fam_links <- function(gedcom, record){
  
  # Family group record has changed
  # Ensure FAMS/FAMC in indi records are correct
  
  # Who are the members of this family?
  spou_xref <- unname(c(record@husb_xref, record@wife_xref)) |> 
    remove_void_xrefs()
  chil_xrefs <- unname(c(record@chil_xrefs)) |> 
    remove_void_xrefs()
  
  for(indi in c(spou_xref, chil_xrefs)){
    if(!indi %in% gedcom@c_xrefs[["indi"]]){
      stop("There is no individual with xref ", indi)
    }
  }
  
  # Ensure these members have the family link
  for(spou in spou_xref){
    spou_rec <- gedcom@indi[[spou]]
    fams <- find_ged_values(spou_rec, "FAMS")
    if(!record@xref %in% fams){
      gedcom@indi[[spou]] <- c(
        gedcom@indi[[spou]],
        sprintf("1 FAMS %s", record@xref)
      )
    }
  }
  
  for(chil in chil_xrefs){
    chil_rec <- gedcom@indi[[chil]]
    famc <- find_ged_values(chil_rec, "FAMC")
    if(!record@xref %in% famc){
      gedcom@indi[[chil]] <- c(
        gedcom@indi[[chil]],
        sprintf("1 FAMC %s", record@xref)
      )
    }
  }
  
  # Ensure no one else has the family link
  for(indi in gedcom@c_xrefs[["indi"]]){
    if(indi %in% c(spou_xref, chil_xrefs)) next
    
    rec <- gedcom@indi[[indi]]
    
    fam_row <- grep(sprintf("^1 (FAMC|FAMS) %s$", record@xref), rec)
    
    if(length(fam_row) > 0){
      # Might want to alert the user if extra stuff is being deleted
      gedcom@indi[[indi]] <- delete_ged_section(rec, fam_row)
    }

  }
  gedcom
}


refresh_indi_links <- function(gedcom, record){
  
  # Individual record has changed
  # Ensure members of family records are correct
  
  # Go through each family link
  # Ensure record@xref is properly reflected in family record membership
  fam_lnks <- find_ged_values(record@c_as_ged, "FAMS|FAMC")
  for(fam in fam_lnks){
    if(!fam %in% gedcom@c_xrefs[["fam"]]){
      stop("There is no family with xref ", fam)
    }
  }
  
  for(lnk in record@fam_links_spou){
    fam_xref <- ifelse(!is.character(lnk), lnk@fam_xref, lnk)
    fam_rec <- gedcom@fam[[fam_xref]]
    
    fam_husb <- find_ged_values(fam_rec, "HUSB")
    fam_wife <- find_ged_values(fam_rec, "WIFE")
    fam_spou <- c(fam_husb, fam_wife)
    
    if(!record@xref %in% fam_spou){
      if(length(fam_spou) == 0){
        # use sex as determinant
        if(record@sex == "M"){
          spou_type <- "HUSB"
        } else {
          spou_type <- "WIFE"
        }
        
      } else if(length(fam_husb) == 0) {
        spou_type <- "HUSB"
      } else if(length(fam_wife) == 0) {
        spou_type <- "WIFE"
      } else {
        # Both a HUSB and WIFE already exist
        stop("This individual cannot be a spouse of a family that already has two spouses.")
      }
      
      gedcom@fam[[fam_xref]] <- c(
        gedcom@fam[[fam_xref]],
        sprintf("1 %s %s", spou_type, record@xref)
      )
    }
  }
  
  for(lnk in record@fam_links_chil){
    fam_xref <- ifelse(!is.character(lnk), lnk@fam_xref, lnk)
    fam_rec <- gedcom@fam[[fam_xref]]
    fam_chil <- find_ged_values(fam_rec, "CHIL")
    
    if(!record@xref %in% fam_chil){
      gedcom@fam[[fam_xref]] <- c(
        gedcom@fam[[fam_xref]],
        sprintf("1 CHIL %s", record@xref)
      )
    }
  } 
  
  # Ensure this individual (record@xref) appears in no other fam records
  for(fam in gedcom@c_xrefs[["fam"]]){
    
    if(fam %in% fam_lnks) next
    
    rec <- gedcom@fam[[fam]]
    
    memb_row <- grep(sprintf("^1 (HUSB|WIFE|CHIL) %s$", record@xref), rec)
    
    if(length(memb_row) > 0){
      # Might want to alert the user if extra stuff is being deleted
      gedcom@fam[[fam]] <- delete_ged_section(rec, memb_row)
    }
    
  }
  
  gedcom
}

order_facts <- function(record){
  
  if(length(record@facts) == 0) return(record)
  
  # get dates of all facts
  dts <- lapply(record@facts, \(fct){
    if(length(fct@date_sort) == 1){
      dt <- obj_to_ged(fct@date_sort, "SDATE")[1]
    } else if(length(fct@date) == 1) {
      dt <- obj_to_ged(fct@date, "DATE")[1]
    } else {
      dt <- ""
    }
    sub("^0 S?DATE ", "", dt) |> 
      stats::setNames(fct@fact_type)
  })
  
  # Convert to date
  dts <- unlist(lapply(dts, parse_gedcom_date))
  
  # TODO: Don't reorder facts of the same type
  
  record@facts <- record@facts[order(dts)]
  
  record
}

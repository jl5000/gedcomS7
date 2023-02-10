
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
    record@last_updated <- class_change_date()
  }
  
  rec_type <- get_record_type(record)
  
  if(record@xref == "@gedcomR7orphan@")
    record@xref <- unname(gedcom@next_xref[rec_type])
  
  R7::prop(gedcom, rec_type)[[record@xref]] <- record@as_ged
  
  if(rec_type == "indi"){
    gedcom <- refresh_indi_links(gedcom, record)
  } else if(rec_type == "famg"){
    gedcom <- refresh_fam_links(gedcom, record)
  }
  
  gedcom
}


refresh_fam_links <- function(gedcom, record){
  
  # Family group record has changed
  # Ensure FAMS/FAMC in indi records are correct
  
  # Who are the members of this family?
  spou_xref <- c(record@husb_xref, record@wife_xref)
  chil_xref <- c(record@chil_biol_xref, record@chil_adop_xref, record@chil_fost_xref)
  
  for(indi in c(spou_xref, chil_xref)){
    if(!indi %in% names(gedcom@indi)){
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
  
  for(chil in chil_xref){
    chil_rec <- gedcom@indi[[chil]]
    famc <- find_ged_values(chil_rec, "FAMC")
    if(!record@xref %in% famc){
      
      if(chil %in% record@chil_adop_xref){
        pedi <- "adopted"
      } else if(chil %in% record@chil_fost_xref){
        pedi <- "foster"
      } else if(chil %in% record@chil_biol_xref){
        pedi <- "birth"
      }
      
      gedcom@indi[[chil]] <- c(
        gedcom@indi[[chil]],
        sprintf("1 FAMC %s", record@xref),
        sprintf("2 PEDI %s", pedi)
      )
    }
  }
  
  # Ensure no one else has the family link
  for(indi in names(gedcom@indi)){
    if(indi %in% c(spou_xref, chil_xref)) next
    
    rec <- gedcom@indi[[indi]]
    
    fam_row <- grep(sprintf("^1 (FAMC|FAMS) %s$", record@xref), rec)
    
    if(length(fam_row) == 1){
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
  famg_lnks <- find_ged_values(record@as_ged, "FAMS|FAMC")
  for(famg in famg_lnks){
    if(!famg %in% names(gedcom@famg)){
      stop("There is no Family Group with xref ", famg)
    }
  }
  
  for(lnk in record@family_links){
    fam_xref <- lnk@xref
    
    fam_rec <- gedcom@famg[[fam_xref]]
    
    if(is_child_link(lnk)){
      fam_chil <- find_ged_values(fam_rec, "CHIL")
      
      if(!record@xref %in% fam_chil){
        gedcom@famg[[fam_xref]] <- c(
          gedcom@famg[[fam_xref]],
          sprintf("1 CHIL %s", record@xref)
        )
      }

    } else if(is_spouse_link(lnk)){
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
        
        gedcom@famg[[fam_xref]] <- c(
          gedcom@famg[[fam_xref]],
          sprintf("1 %s %s", spou_type, record@xref)
        )
      }
    }
  }
  
  # Ensure this individual (record@xref) appears in no other famg records
  for(famg in names(gedcom@famg)){
    
    if(famg %in% famg_lnks) next
    
    rec <- gedcom@famg[[famg]]
    
    memb_row <- grep(sprintf("^1 (HUSB|WIFE|CHIL) %s$", record@xref), rec)
    
    if(length(memb_row) == 1){
      gedcom@famg[[famg]] <- delete_ged_section(rec, memb_row)
    }
    
  }
  
  gedcom
}



#' Place an edited record object back into a GEDCOM object
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
  
  if(R7::R7_inherits(record, class_record_indi)){
    gedcom@indi[[record@xref]] <- record
    gedcom <- refresh_indi_links(gedcom, record)
  } else if(R7::R7_inherits(record, class_record_famg)){
    gedcom@famg[[record@xref]] <- record
    gedcom <- refresh_fam_links(gedcom, record)
  } else if(R7::R7_inherits(record, class_record_sour)){
    gedcom@sour[[record@xref]] <- record
  } else if(R7::R7_inherits(record, class_record_repo)){
    gedcom@repo[[record@xref]] <- record
  } else if(R7::R7_inherits(record, class_record_media)){
    gedcom@media[[record@xref]] <- record
  } else if(R7::R7_inherits(record, class_record_note)){
    gedcom@note[[record@xref]] <- record
  } else {
    stop("Unrecognised record")
  }
  gedcom
}


refresh_fam_links <- function(gedcom, record){
  
  # Family group record has changed
  # Ensure FAMS/FAMC in indi records are correct
  
  # Who are the members of this family?
  spou_xref <- c(record@husb_xref, record@wife_xref)
  chil_xref <- record@chil_xref
  
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
      gedcom@indi[[chil]] <- c(
        gedcom@indi[[chil]],
        sprintf("1 FAMC %s", record@xref),
        sprintf("2 PEDI %s", "birth") #TODO: Assume birth for now
      )
    }
  }
  
  # Ensure no one else has the family link
  for(indi in names(gedcom@indi)){
    if(indi %in% c(record@husb_xref, record@wife_xref, record@chil_xref)) next
    
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
  
}


refresh_spouse_links <- function(x, modify_famg_members){
  
  xdf <- x@as_df
  
  spou_links <- dplyr::filter(xdf, level == 1,
                              tag %in% c("HUSB","WIFE","FAMS")) |>
    dplyr::mutate(famg = dplyr::if_else(tag == "FAMS", value,  record)) |>
    dplyr::mutate(indi = dplyr::if_else(tag == "FAMS", record, value)) |>
    dplyr::mutate(pair = paste(famg, indi)) |>
    dplyr::add_count(pair) |>
    dplyr::filter(n < 2)
  
  # spou_links2 <- xdf[level == 1 & tag %chin% c("HUSB","WIFE","FAMS")
  #                   ][,famg:= ifelse(tag == "FAMS", value,  record)
  #                     ][,indi:= ifelse(tag == "FAMS", record,  value)
  #                       ][,pair:= paste(famg, indi)
  #                         ][, .SD[.N < 2], by = pair]
  
  for(i in seq_len(nrow(spou_links))){
    tag <- spou_links$tag[i]
    indi <- spou_links$indi[i]
    famg <- spou_links$famg[i]
    husb <- x@get_famg[[famg]]@husb_xref
    wife <- x@get_famg[[famg]]@wife_xref
    
    if(tag == "FAMS"){ # add spouse to famg
      
      if(modify_famg_members){
        
        if(length(c(husb, wife)) == 0){
          # use sex as determinant
          if(x@get_indi[[indi]]@sex == "M"){
            x@famg[[famg]]@husb_xref <- indi
          } else {
            x@famg[[famg]]@wife_xref <- indi
          }
          
        } else if(length(husb) == 0) {
          x@famg[[famg]]@husb_xref <- indi
        } else {
          x@famg[[famg]]@wife_xref <- indi
        }
        
      } else { # remove indi spouse link
        
      }
      
      
      
    } else { # remove spouse from famg
      
      if(modify_famg_members){
        if(husb == indi){
          x@famg[[famg]]@husb_xref <- character()
        } else if (wife == indi){
          x@famg[[famg]]@wife_xref <- character()
        }
      } else { # add indi spouse link
        x@indi[[indi]] <- add_indi_family_link(x@get_indi[[indi]], famg, as_child = FALSE)
      }
      
      
    }
    
  }
  
  x
}

refresh_child_links <- function(x, modify_famg_members){
  
  xdf <- x@as_df
  
  chil_links <- dplyr::filter(xdf, level == 1,
                              tag %in% c("CHIL","FAMC")) |>
    dplyr::mutate(famg = dplyr::if_else(tag == "FAMC", value,  record)) |>
    dplyr::mutate(indi = dplyr::if_else(tag == "FAMC", record, value)) |>
    dplyr::mutate(pair = paste(famg, indi)) |>
    dplyr::add_count(pair) |>
    dplyr::filter(n < 2)
  
  for(i in seq_len(nrow(chil_links))){
    tag <- chil_links$tag[i]
    indi <- chil_links$indi[i]
    famg <- chil_links$famg[i]
    chil <- x@get_famg[[famg]]@chil_xref
    
    if(tag == "FAMC"){ 
      
      if(modify_famg_members){ # add child to famg
        x@famg[[famg]]@chil_xref <- c(chil, indi)
      } else { # remove indi family link
        
      }
      
      
    } else { # tag = "CHIL"
      
      if(modify_famg_members){ # remove child from famg
        x@famg[[famg]]@chil_xref <- chil[chil != indi]
      } else { # add indi family link
        x@indi[[indi]] <- add_indi_family_link(x@get_indi[[indi]], famg, as_child = TRUE)
      }
      
      
    }
    
  }
  
  x
}


# 
# 
# for(fam in famg){
#   for(spou in c(fam@husb_xref, fam@wife_xref)){
#     
#     x@indi[[spou]] <- add_indi_family_link(x@indi[[spou]],
#                                            xref_famg = xref,
#                                            as_child = FALSE)
#   }
#   
#   if(is.null(names(chil_xref)))
#     names(chil_xref) <- rep("birth", length(chil_xref))
#   
#   for(chil in fam@chil_xref){
#     ped <- names(chil_xref[chil_xref == chil])
#     x@indi[[chil]] <- add_indi_family_link(x@indi[[chil]],
#                                            xref_famg = xref,
#                                            as_child = TRUE,
#                                            pedigree = ped)
#   }
# }
# 
# 
# 
# 

# 
# for(fam in famg){
#   for(spou in c(fam@husb_xref, fam@wife_xref)){
#     
#     x@indi[[spou]] <- add_indi_family_link(x@indi[[spou]],
#                                            xref_famg = xref,
#                                            as_child = FALSE)
#   }
#   
#   if(is.null(names(chil_xref)))
#     names(chil_xref) <- rep("birth", length(chil_xref))
#   
#   for(chil in fam@chil_xref){
#     ped <- names(chil_xref[chil_xref == chil])
#     x@indi[[chil]] <- add_indi_family_link(x@indi[[chil]],
#                                            xref_famg = xref,
#                                            as_child = TRUE,
#                                            pedigree = ped)
#   }
# }

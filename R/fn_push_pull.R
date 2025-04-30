
#' Pull a record from a GEDCOM object for editing
#' 
#' @details The record is not removed from the gedcom object, rather a copy is taken.
#'
#' @param x A gedcom object.
#' @param xref The xref of the record to pull.
#'
#' @returns An S7 object representing the record.
#' @export
#' @tests
#' expect_error(pull_record(new_gedcom(), "@I1@"),
#'              regexp = "^The xref is not in the GEDCOM")
#' 
#' ged <- new_gedcom() |> 
#'          push_record(IndividualRecord(locked = TRUE)) |> 
#'          suppressMessages()
#' expect_warning(pull_record(ged, "@I1@"),
#'                regexp = "^The record is locked")
#'                
#' ged <- new_gedcom() |> 
#'          push_record(IndividualRecord(
#'             facts = IndividualEvent("BIRT", locked = TRUE))) |> 
#'          suppressMessages()
#' expect_warning(pull_record(ged, "@I1@"),
#'                regexp = "^The following facts are locked")
#'                
#' ged <- new_gedcom() |> 
#'          push_record(IndividualRecord(sex = "M")) |> 
#'          suppressMessages()
#' ged@records@RAW@INDI[[1]] <- c(ged@records@RAW@INDI[[1]], "1 _FRE Wow", "2 _ERR No")
#' expect_warning(pull_record(ged, "@I1@"),
#'                regexp = "The following lines could not be parsed.+Wow.+No")
pull_record <- function(x, xref){
  
  if(!xref %in% unlist(x@records@XREFS))
    stop("The xref is not in the GEDCOM object")
  
  rec_lines <- c(x@records@RAW@INDI, x@records@RAW@FAM, 
                 x@records@RAW@SOUR, x@records@RAW@REPO, 
                 x@records@RAW@OBJE, x@records@RAW@SNOTE, 
                 x@records@RAW@SUBM)[[xref]]
  
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
      
      if(any(locked_facts))
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
#' @returns An updated GEDCOM object.
#' @export
#' @tests
#' ged <- new_gedcom()
#' ged@update_change_dates <- TRUE
#' ged@add_creation_dates <- TRUE
#' expect_message(ged <- push_record(ged, IndividualRecord()),
#'                regexp = "New Individual record added with xref @I1@")
#' expect_true("1 CHAN" %in% ged@records@RAW@INDI[[1]])
#' expect_true("1 CREA" %in% ged@records@RAW@INDI[[1]])
#' 
#' ged <- new_gedcom()
#' 
#' expect_error(push_record(ged, FamilyRecord(chil_xrefs = "@I1@")),
#'              regexp = "^The following xrefs were not found in the GEDCOM object: @I1@")
#' expect_error(push_record(ged, IndividualRecord(fam_links_spou = "@F1@",
#'                                                media_links = "@O1@")),
#'              regexp = "^The following xrefs were not found in the GEDCOM object: @F1@, @O1@")
#'              
#' suppressMessages({
#'   ged <- push_record(ged, FamilyRecord())
#'   ged <- push_record(ged, FamilyRecord())
#'   ged <- push_record(ged, IndividualRecord(fam_links_spou = "@F2@"))
#'   ged <- push_record(ged, IndividualRecord(fam_links_chil = "@F1@"))
#' })
#' 
#' expect_true("1 HUSB @I1@" %in% ged@records@RAW@FAM[["@F2@"]])
#' expect_true("1 CHIL @I2@" %in% ged@records@RAW@FAM[["@F1@"]])
#' 
#' suppressMessages({
#'   rec_F1 <- pull_record(ged, "@F1@")
#'   rec_F1@chil_xrefs <- "@I1@"
#'   ged <- push_record(ged, rec_F1)
#'   
#'   rec_F2 <- pull_record(ged, "@F2@")
#'   rec_F2@husb_xref <- "@I2@"
#'   ged <- push_record(ged, rec_F2)
#' })
#' 
#' expect_true("1 FAMC @F1@" %in% ged@records@RAW@INDI[["@I1@"]])
#' expect_true("1 FAMS @F2@" %in% ged@records@RAW@INDI[["@I2@"]])
#' expect_false("1 FAMC @F1@" %in% ged@records@RAW@INDI[["@I2@"]])
#' expect_false("1 FAMS @F2@" %in% ged@records@RAW@INDI[["@I1@"]])
#' expect_false("1 CHIL @I2@" %in% ged@records@RAW@FAM[["@F1@"]])
#' expect_false("1 HUSB @I1@" %in% ged@records@RAW@FAM[["@F2@"]])
push_record <- function(gedcom, record){
  
  check_missing_xrefs(gedcom, record)
  
  if(gedcom@update_change_dates){
     record@updated <- ChangeDate()
  }
  
  if(gedcom@add_creation_dates){
    if(length(record@created) == 0 && record@XREF == "@GEDCOMS7_ORPHAN@"){
      record@created <- CreationDate()
    }
  }
  
  rec_type <- get_record_type(record)
  new_rec <- record@XREF == "@GEDCOMS7_ORPHAN@"
  
  if(new_rec)
    record@XREF <- gedcom@records@XREFS_NEXT[[rec_type]]
  
  # Don't do this yet
  #if(rec_type %in% c("INDI","FAM")) record <- order_facts(record)
  
  lines <- record@GEDCOM
  S7::prop(gedcom@records@RAW, rec_type)[[record@XREF]] <- lines
  
  if(rec_type == "INDI"){
    gedcom <- refresh_indi_links(gedcom, record)
  } else if(rec_type == "FAM"){
    gedcom <- refresh_fam_links(gedcom, record)
  }
  
  if(new_rec)
    message("New ", names(which(val_record_types() == rec_type)), " record added with xref ", record@XREF)
  
  gedcom
}


check_missing_xrefs <- function(gedcom, record){
  
  lines <- record@GEDCOM
  line_vals <- parse_line_value(lines)
  xref_ptrs <- line_vals[grepl(reg_xref(TRUE), line_vals)]
  xref_ptrs <- remove_void_xrefs(xref_ptrs)
  missing_xrefs <- setdiff(xref_ptrs, unlist(gedcom@records@XREFS))
  
  if(length(missing_xrefs) > 0)
    stop("The following xrefs were not found in the GEDCOM object: ", 
         toString(missing_xrefs))
  
}

refresh_fam_links <- function(gedcom, record){
  
  # Family record has changed
  # Ensure FAMS/FAMC in indi records are correct
  
  # Who are the members of this family?
  spou_xref <- unname(c(record@husb_xref, record@wife_xref)) |> 
    remove_void_xrefs()
  chil_xrefs <- unname(c(record@chil_xrefs)) |> 
    remove_void_xrefs()
  
  # Ensure these members have the family link
  for(spou in spou_xref){
    spou_rec <- gedcom@records@RAW@INDI[[spou]]
    fams <- find_ged_values(spou_rec, "FAMS")
    if(!record@XREF %in% fams){
      gedcom@records@RAW@INDI[[spou]] <- c(
        gedcom@records@RAW@INDI[[spou]],
        sprintf("1 FAMS %s", record@XREF)
      )
    }
  }
  
  for(chil in chil_xrefs){
    chil_rec <- gedcom@records@RAW@INDI[[chil]]
    famc <- find_ged_values(chil_rec, "FAMC")
    if(!record@XREF %in% famc){
      gedcom@records@RAW@INDI[[chil]] <- c(
        gedcom@records@RAW@INDI[[chil]],
        sprintf("1 FAMC %s", record@XREF)
      )
    }
  }
  
  # Ensure no one else has the family link
  for(indi in gedcom@records@XREFS[["INDI"]]){
    if(indi %in% c(spou_xref, chil_xrefs)) next
    
    gedcom@records@RAW@INDI[[indi]] <- delete_ged_sections(
      gedcom@records@RAW@INDI[[indi]],
      \(x) grep(sprintf("^1 (FAMC|FAMS) %s$", record@XREF), x)
    )

  }
  gedcom
}


refresh_indi_links <- function(gedcom, record){
  
  # Individual record has changed
  # Ensure members of family records are correct
  
  # Go through each family link
  # Ensure record@XREF is properly reflected in family record membership

  for(lnk in record@fam_links_spou){
    fam_xref <- lnk@fam_xref
    if(fam_xref == "@VOID@") next
    fam_rec <- gedcom@records@RAW@FAM[[fam_xref]]
    
    fam_husb <- find_ged_values(fam_rec, "HUSB")
    fam_wife <- find_ged_values(fam_rec, "WIFE")
    fam_spou <- c(fam_husb, fam_wife)
    
    if(!record@XREF %in% fam_spou){
      if(length(fam_spou) == 0){
        # use sex as determinant
        if(record@sex == "F"){
          spou_type <- "WIFE"
        } else {
          spou_type <- "HUSB"
        }
        
      } else if(length(fam_husb) == 0) {
        spou_type <- "HUSB"
      } else if(length(fam_wife) == 0) {
        spou_type <- "WIFE"
      } else {
        # Both a HUSB and WIFE already exist
        stop("This individual cannot be a spouse of a family that already has two spouses.")
      }
      
      gedcom@records@RAW@FAM[[fam_xref]] <- c(
        gedcom@records@RAW@FAM[[fam_xref]],
        sprintf("1 %s %s", spou_type, record@XREF)
      )
    }
  }
  
  for(lnk in record@fam_links_chil){
    fam_xref <- lnk@fam_xref
    if(fam_xref == "@VOID@") next
    fam_rec <- gedcom@records@RAW@FAM[[fam_xref]]
    fam_chil <- find_ged_values(fam_rec, "CHIL")
    
    if(!record@XREF %in% fam_chil){
      gedcom@records@RAW@FAM[[fam_xref]] <- c(
        gedcom@records@RAW@FAM[[fam_xref]],
        sprintf("1 CHIL %s", record@XREF)
      )
    }
  } 
  
  # Ensure this individual (record@XREF) appears in no other fam records
  fam_lnks <- find_ged_values(record@GEDCOM, "FAMS|FAMC")
  
  for(fam in gedcom@records@XREFS[["FAM"]]){
    
    if(fam %in% fam_lnks) next
    
    gedcom@records@RAW@FAM[[fam]] <- delete_ged_sections(
      gedcom@records@RAW@FAM[[fam]],
      \(x) grep(sprintf("^1 (HUSB|WIFE|CHIL) %s$", record@XREF), x)
    )
    
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

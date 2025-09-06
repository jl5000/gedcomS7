
#' Add parent records for an individual
#' 
#' This function adds records for an individual's parents.
#' 
#' @details This function may also create a Family record and will 
#' not modify existing parents.
#'
#' @param x A gedcom object.
#' @param xref The xref of an Individual record.
#' @param inc_sex Whether to populate the sex of the parents. This will ensure
#' that there is one male and one female parent. Otherwise the sex will be
#' assigned as "U" (undetermined).
#' @param fath_name,moth_name Optional names to give to the parents. 
#' Surnames must be enclosed in forward slashes.
#'
#' @returns A gedcom object with additional parent records.
#' @export
#' @tests
#' ged <- new_gedcom()
#' 
#' ged_no_parents <- push_record(ged, IndividualRecord()) |> 
#'                   suppressMessages()
#' ged_with_parents <- add_parents(ged_no_parents, "@I1@") |> 
#'                   suppressMessages()
#' expect_true("1 SEX M" %in% ged_with_parents@records@RAW@INDI[["@I2@"]])
#' expect_true("1 SEX F" %in% ged_with_parents@records@RAW@INDI[["@I3@"]])
#' expect_true("1 CHIL @I1@" %in% ged_with_parents@records@RAW@FAM[["@F1@"]])
#' expect_true("1 HUSB @I2@" %in% ged_with_parents@records@RAW@FAM[["@F1@"]])
#' expect_true("1 WIFE @I3@" %in% ged_with_parents@records@RAW@FAM[["@F1@"]])
#' 
#' ged_with_parents <- add_parents(ged_no_parents, "@I1@", inc_sex = FALSE) |> 
#'                   suppressMessages()
#' expect_false("1 SEX M" %in% ged_with_parents@records@RAW@INDI[["@I2@"]])
#' expect_false("1 SEX F" %in% ged_with_parents@records@RAW@INDI[["@I3@"]])
#' 
#' ged_with_parents <- add_parents(ged_no_parents, "@I1@",
#'                                 fath_name = "Joe /Bloggs/",
#'                                 moth_name = "Jess /Bloggs/") |> 
#'                   suppressMessages()
#' expect_true("1 NAME Joe /Bloggs/" %in% ged_with_parents@records@RAW@INDI[["@I2@"]])
#' expect_true("1 NAME Jess /Bloggs/" %in% ged_with_parents@records@RAW@INDI[["@I3@"]])
#' 
#' ged_with_parents2 <- add_parents(ged_with_parents, "@I1@") |> 
#'                   suppressMessages()
#' expect_equal(ged_with_parents@GEDCOM, ged_with_parents2@GEDCOM)
#' expect_warning(add_parents(ged_with_parents, "@I1@", fath_name = "Me"),
#'                regexp = "^Father name not used")
#' expect_warning(add_parents(ged_with_parents, "@I1@", moth_name = "Me"),
#'                regexp = "^Mother name not used")
#' 
#' ged_one_parent_f <- suppressMessages(
#'   ged |> 
#'     push_record(IndividualRecord()) |>
#'     push_record(IndividualRecord(sex = "M")) |> 
#'     push_record(FamilyRecord(chil_xrefs = "@I1@", husb_xref = "@I2@"))
#' )
#' ged_with_parents <- add_parents(ged_one_parent_f, "@I1@") |> 
#'                   suppressMessages()
#' expect_true("1 SEX M" %in% ged_with_parents@records@RAW@INDI[["@I2@"]])
#' expect_true("1 SEX F" %in% ged_with_parents@records@RAW@INDI[["@I3@"]])
#' expect_true("1 CHIL @I1@" %in% ged_with_parents@records@RAW@FAM[["@F1@"]])
#' expect_true("1 HUSB @I2@" %in% ged_with_parents@records@RAW@FAM[["@F1@"]])
#' expect_true("1 WIFE @I3@" %in% ged_with_parents@records@RAW@FAM[["@F1@"]])
#' 
#' ged_one_parent_m <- suppressMessages(
#'   ged |> 
#'     push_record(IndividualRecord()) |>
#'     push_record(IndividualRecord(sex = "F")) |> 
#'     push_record(FamilyRecord(chil_xrefs = "@I1@", wife_xref = "@I2@"))
#' )
#' ged_with_parents <- add_parents(ged_one_parent_m, "@I1@") |> 
#'                   suppressMessages()
#' expect_true("1 SEX F" %in% ged_with_parents@records@RAW@INDI[["@I2@"]])
#' expect_true("1 SEX M" %in% ged_with_parents@records@RAW@INDI[["@I3@"]])
#' expect_true("1 CHIL @I1@" %in% ged_with_parents@records@RAW@FAM[["@F1@"]])
#' expect_true("1 WIFE @I2@" %in% ged_with_parents@records@RAW@FAM[["@F1@"]])
#' expect_true("1 HUSB @I3@" %in% ged_with_parents@records@RAW@FAM[["@F1@"]])
add_parents <- function(x, 
                        xref, 
                        inc_sex = TRUE, 
                        fath_name = NULL, 
                        moth_name = NULL){
  
  check_indi_rec(x, xref)
  
  # Get first family xref as child - cannot assume there will be just one
  famc_xref <- get_fam_as_child(x, xref, "BIRTH")
  if(length(famc_xref) > 0) famc_xref <- famc_xref[1]
  
  # Create new family record if necessary
  if(length(famc_xref) == 0){
    famc_xref <- x@records@XREFS_NEXT[["FAM"]]
    
    famc_rec <- FamilyRecord(
      chil_xrefs = xref
    )
    
    x <- push_record(x, famc_rec)
  }
  
  moth_xref <- find_ged_values(x@records@RAW@FAM[[famc_xref]], "WIFE")
  fath_xref <- find_ged_values(x@records@RAW@FAM[[famc_xref]], "HUSB")
  
  if(length(moth_xref) > 0 && !is.null(moth_name))
    warning("Mother name not used as mother already exists")
  
  if(length(fath_xref) > 0 && !is.null(fath_name))
    warning("Father name not used as father already exists")
  
  if(length(fath_xref) > 0 && length(moth_xref) > 0)
    return(x)
    
  # Add father
  if(length(fath_xref) == 0){
    
    par_rec <- IndividualRecord(
      fam_links_spou = famc_xref
    )
    if(inc_sex) par_rec@sex <- "M"
    if(!is.null(fath_name)) par_rec@pers_names <- fath_name
    
    x <- push_record(x, par_rec)
  }
  
  # Add mother
  if(length(moth_xref) == 0){
    
    par_rec <- IndividualRecord(
      fam_links_spou = famc_xref
    )
    if(inc_sex) par_rec@sex <- "F"
    if(!is.null(moth_name)) par_rec@pers_names <- moth_name
    
    x <- push_record(x, par_rec)
  }
  
  x
}


#' Add a spouse record for an individual
#' 
#' This creates a record for a spouse and potentially their Family record.
#'
#' @param x A gedcom object.
#' @param xref The xref of an Individual record.
#' @param sex The sex of the spouse.
#' @param spou_name Optional name to give to the spouse.
#' Surnames must be enclosed in forward slashes.
#' @param fam_xref The cross-reference identifier of the Family record
#' if it already exists. If this is not provided, a new Family record will
#' be created.
#'
#' @returns A gedcom object with additional spouse and Family records.
#' @export
#' @tests
#' ged <- new_gedcom()
#' 
#' ged_no_spouse <- push_record(ged, IndividualRecord()) |> 
#'                   suppressMessages()
#' ged_with_spouse <- add_spouse(ged_no_spouse, "@I1@") |> 
#'                   suppressMessages()
#' 
#' expect_true("1 SEX U" %in% ged_with_spouse@records@RAW@INDI[["@I2@"]])
#' expect_true("1 HUSB @I1@" %in% ged_with_spouse@records@RAW@FAM[["@F1@"]])
#' expect_true("1 WIFE @I2@" %in% ged_with_spouse@records@RAW@FAM[["@F1@"]])
#' 
#' ged_with_spouse <- add_spouse(ged_no_spouse, "@I1@",
#'                               spou_name = "Joe /Bloggs/") |> 
#'                   suppressMessages()
#' 
#' expect_true("1 NAME Joe /Bloggs/" %in% ged_with_spouse@records@RAW@INDI[["@I2@"]])
#' 
#' ged_no_spouse_fam <- suppressMessages(
#'   ged |> 
#'     push_record(IndividualRecord()) |> 
#'     push_record(FamilyRecord(husb_xref = "@I1@"))
#' )
#' ged_with_spouse_fam_new <- add_spouse(ged_no_spouse_fam, "@I1@") |> 
#'                   suppressMessages()
#' ged_with_spouse_fam <- add_spouse(ged_no_spouse_fam, "@I1@", fam_xref = "@F1@") |> 
#'                   suppressMessages()
#' 
#' expect_equal(length(ged_with_spouse_fam_new@records@RAW@FAM), 2)
#' expect_equal(length(ged_with_spouse_fam@records@RAW@FAM), 1)
#' expect_true("1 HUSB @I1@" %in% ged_with_spouse_fam_new@records@RAW@FAM[["@F1@"]])
#' expect_true("1 HUSB @I1@" %in% ged_with_spouse_fam_new@records@RAW@FAM[["@F2@"]])
#' 
#' expect_false("1 WIFE @I2@" %in% ged_with_spouse_fam_new@records@RAW@FAM[["@F1@"]])
#' expect_true("1 WIFE @I2@" %in% ged_with_spouse_fam_new@records@RAW@FAM[["@F2@"]])
#' 
#' expect_true("1 HUSB @I1@" %in% ged_with_spouse_fam@records@RAW@FAM[["@F1@"]])
#' expect_true("1 WIFE @I2@" %in% ged_with_spouse_fam@records@RAW@FAM[["@F1@"]])
add_spouse <- function(x, xref, sex = "U", spou_name = NULL, fam_xref = NULL){
  check_indi_rec(x, xref)
  if(!is.null(fam_xref)) check_fam_rec(x, fam_xref)
  
  spou_xref <- x@records@XREFS_NEXT[["INDI"]]
  
  spou_rec <- IndividualRecord(
    sex = sex
  )
  if(!is.null(spou_name)) spou_rec@pers_names <- spou_name
  if(!is.null(fam_xref)) spou_rec@fam_links_spou <- fam_xref
  
  x <- push_record(x, spou_rec)
  if(!is.null(fam_xref)) return(x)
  
  if(sex == "M"){
    husb_xref = spou_xref
    wife_xref = xref
  } else if(sex == "F"){
    husb_xref = xref
    wife_xref = spou_xref
  } else {
    curr_sex <- find_ged_values(x@records@RAW@INDI[[xref]], "SEX")
    if(length(curr_sex) == 1 && curr_sex == "F"){
      husb_xref = spou_xref
      wife_xref = xref
    } else {
      husb_xref = xref
      wife_xref = spou_xref
    }
  }
  
  fams_rec <- FamilyRecord(
    husb_xref = husb_xref,
    wife_xref = wife_xref
  )
  
  push_record(x, fams_rec)
}



#' Create sibling records for an individual
#' 
#' @details This function may also create a Family record and will 
#' not modify existing siblings.
#'
#' @param x A gedcom object.
#' @param xref The xref of an Individual record.
#' @param sexes A character string giving the sexes of each sibling. For example,
#' "FFM" to add two sisters and one brother.
#' @param sib_names A character vector of sibling's names. If provided, it must be
#' the same length as the number of sexes. If you don't want to provide a name for a
#' sibling, set the name to "". 
#' 
#' Surnames must be enclosed in forward slashes. If all names you supply do not
#' contain forward slashes then surnames will be taken from the father (or mother).
#'
#' @returns A gedcom object with additional sibling records.
#' @export
add_siblings <- function(x, xref, sexes, sib_names = NULL){
  check_indi_rec(x, xref)
  if(!is.null(sib_names) && nchar(sexes) != length(sib_names))
    stop("If sibling names are given, the length of sib_names must be equal to the number of sexes given.")
  
  famc_xref <- get_fam_as_child(x, xref, "BIRTH")
  
  if(length(famc_xref) == 0){
    famc_xref <- x@records@XREFS_NEXT[["FAM"]]
    
    famc_rec <- FamilyRecord(
      chil_xrefs = xref
    )
    
    x <- push_record(x, famc_rec)
  }
  
  add_children(x, famc_xref, sexes, sib_names)
}

#' Create children records for a family
#'
#' @param x A gedcom object.
#' @param xref The xref of a Family record.
#' @param sexes A character string giving the sexes of each child. For example,
#' "FFM" to add two daughters and one son.
#' @param chil_names A character vector of children's names. If provided, it must be
#' the same length as the number of sexes. If you don't want to provide a name for a
#' child, set the name to "". 
#' 
#' Surnames must be enclosed in forward slashes. If all names you supply do not
#' contain forward slashes then surnames will be taken from the father (or mother).
#'
#' @returns A gedcom object with additional child records.
#' @export
add_children <- function(x, xref, sexes, chil_names = NULL){
  check_fam_rec(x, xref)
  if(!is.null(chil_names) && nchar(sexes) != length(chil_names))
    stop("If child names are given, the length of chil_names must be equal to the number of sexes given.")
  
  # Add surnames from parent if no surnames given
  if(!is.null(chil_names) && !any(grepl("/", chil_names))){
    fath_xref <- find_ged_values(x@records@RAW@FAM[[xref]], "HUSB")
    moth_xref <- find_ged_values(x@records@RAW@FAM[[xref]], "WIFE")
    par_xref <- c(fath_xref, moth_xref)
    if(length(par_xref) > 0){
      par_xref <- par_xref[1] # father or mother
      par_name <- find_ged_values(x@records@RAW@INDI[[par_xref]], "NAME")
      if(length(par_name) > 0){
        par_name <- par_name[1]
      } else {
        par_name <- "None"
      }
      family_name <- sub("^.*/(.*)/.*$", "\\1", par_name)
      if(family_name != par_name){
        chil_names[chil_names != ""] <- sprintf("%s /%s/", 
                                                chil_names[chil_names != ""], 
                                                family_name)
      }
    }
  }
  
  sexes_vec <- unlist(strsplit(sexes, split = NULL))
  
  for(i in seq_along(sexes_vec)){
    if(!sexes_vec[i] %in% val_sexes()){
      warning("Skipping person with unknown sex: ", sexes_vec[i])
      next
    }
    
    chil_rec <- IndividualRecord(
      sex = sexes_vec[i],
      fam_links_chil = xref
    )
    if(!is.null(chil_names) && chil_names[i] != ""){
      chil_rec@pers_names <- chil_names[i]
    }

    x <- push_record(x, chil_rec)
  }
  
  x
}




#' Remove records from a GEDCOM object
#'
#' @param x A gedcom object.
#' @param xrefs A character vector of xrefs to remove.
#' @param void_refs Whether to replace references to this record with
#' a @VOID@ reference. This indicates to people that there was a reference
#' to a record here. Note that if this is set to FALSE, you risk losing
#' supplementary information (e.g. pedigree data in family links).
#'
#' @returns The gedcom object with the records removed.
#' @export
rm_records <- function(x, xrefs, void_refs = TRUE){
  xrefs <- unique(xrefs)
  for(xref in xrefs){
    for(rec_type in names(x@records@prefixes)){
      
      # Delete the record (if it is this type of record)
      S7::prop(x@records@RAW, rec_type)[[xref]] <- NULL
      
      # Delete the pointers to it
      S7::prop(x@records@RAW, rec_type) <- S7::prop(x@records@RAW, rec_type) |> 
        lapply(\(lines) rm_xref_ptrs(lines, xref, void_refs))
    }
  }
  x
}


#' Remove xref pointers from GEDCOM lines
#'
#' @param lines A character vector of GEDCOM lines.
#' @param xref The xref to remove.
#' @param void_refs Whether to replace references to the xref with
#' a @VOID@ reference, or remove the structure entirely.
#'
#' @returns The GEDCOM lines without pointers to the xref.
#' @keywords internal
#' @tests
#' lines1 <- NonEvent(
#'   "NATU", 
#'   date_period = "FROM 1700 TO 1800", 
#'   date_phrase = "No date phrase", 
#'   note_xrefs = "@N1@", 
#'   notes = "Note text", 
#'   citations = list(
#'     SourceCitation("@S1@", where = "1"),
#'     SourceCitation("@S1@", where = "2")
#'   )
#' )@GEDCOM
#' void_lines <- rm_xref_ptrs(lines1, "@S1@", TRUE)
#' expect_equal(parse_line_value(void_lines)[6], void_xref())
#' expect_equal(parse_line_value(void_lines)[8], void_xref())
#' expect_length(rm_xref_ptrs(lines1, "@S1@", FALSE), 5)
#' void_lines <- rm_xref_ptrs(lines1, "@N1@", TRUE)
#' expect_equal(parse_line_value(void_lines)[5], void_xref())
#' expect_length(rm_xref_ptrs(lines1, "@N1@", FALSE), 8)
rm_xref_ptrs <- function(lines, xref, void_refs){
  ptr_rows <- \(lines) which(parse_line_value(lines) == xref)
  rows <- ptr_rows(lines)
  if(length(rows) == 0) return(lines)
  
  if(void_refs){
    lines[rows] <- sub(xref, void_xref(), lines[rows])
  } else {
    lines <- delete_ged_sections(lines, ptr_rows)
  }
  
  lines
}

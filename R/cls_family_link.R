
FamilyLink <- S7::new_class(
  "FamilyLink", 
  parent = GedcomS7class, 
  abstract = TRUE,
  properties = list(
    fam_xref = prop_char(1, 1, pattern = reg_xref(TRUE)),
    note_xrefs = prop_char(pattern = reg_xref(TRUE)),
    notes = prop_S7list("notes", Note)
  )
)

#' Create a family link (as spouse) object
#' 
#' @inheritParams prop_definitions 
#' @returns An S7 object representing a GEDCOM family link as a spouse.
#' @export
#' @tests
#' expect_error(FamilyLinkSpouse(), regexp = "@fam_xref has too few elements")
#' expect_snapshot_value(FamilyLinkSpouse("@F123@")@GEDCOM, "json2")
#' expect_snapshot_value(FamilyLinkSpouse("@F2@", 
#'                                                notes = list(Note("test")))@GEDCOM, "json2")
FamilyLinkSpouse <- S7::new_class(
  "FamilyLinkSpouse",
  parent = FamilyLink,
  properties = list(
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 FAMS %s", self@fam_xref),
          notes_ged(self@notes, self@note_xrefs) |> level_up(1)
        )
      })
  )
)

#' Create a family link (as child) object
#' 
#' @inheritParams prop_definitions 
#' @param pedigree An optional value from `val_pedigree_types()` indicating the nature
#' of the link.
#' @param pedigree_phrase An optional free-text phrase describing the nature of the
#' link. This is required if `@pedigree` is "OTHER".
#' @param confidence An optional value from `val_confidence_types()` indicating the
#' confidence of the link.
#' @param confidence_phrase An optional free-text phrase expanding on the confidence of the
#' link.
#' 
#' @returns An S7 object representing a GEDCOM family link as a child.
#' @export
#' @tests
#' expect_error(FamilyLinkChild("@F123@", pedigree = "father"), 
#'                                      regexp = "@pedigree has an invalid value")
#' expect_error(FamilyLinkChild("@F123@", pedigree = "OTHER"), 
#'                                      regexp = "A @pedigree_phrase must be given if @pedigree is 'OTHER'")
#' expect_error(FamilyLinkChild("@F123@", confidence = "LOW"), 
#'                                      regexp = "@confidence has an invalid value")  
#' expect_error(FamilyLinkChild("@F123@", confidence_phrase = "Don't know"), 
#'                                      regexp = "@confidence_phrase requires a @confidence")                                  
#' expect_snapshot_value(FamilyLinkChild("@F2@", 
#'                                                pedigree = "ADOPTED",
#'                                                pedigree_phrase = "By people",
#'                                                confidence = "CHALLENGED",
#'                                                confidence_phrase = "By someone",
#'                                                note_xrefs = c("@242@","@GJFJ@"))@GEDCOM, "json2")
FamilyLinkChild <- S7::new_class(
  "FamilyLinkChild", 
  parent = FamilyLink,
  properties = list(
    pedigree = prop_char(0, 1, choices = val_pedigree_types()),
    pedigree_phrase = prop_char(0, 1, 1),
    confidence = prop_char(0, 1, choices = val_confidence_types()),
    confidence_phrase = prop_char(0, 1, 1),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 FAMC %s", self@fam_xref),
          sprintf("1 PEDI %s", self@pedigree),
          sprintf("2 PHRASE %s", self@pedigree_phrase),
          sprintf("1 STAT %s", self@confidence),
          sprintf("2 PHRASE %s", self@confidence_phrase),
          notes_ged(self@notes, self@note_xrefs) |> level_up(1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_phrase(self@pedigree_phrase, "@pedigree_phrase",
                       self@pedigree, "@pedigree", "OTHER"),
      chk_input_parents(self@pedigree_phrase, "@pedigree_phrase", self@pedigree, "@pedigree"),
      chk_input_parents(self@confidence_phrase, "@confidence_phrase", self@confidence, "@confidence")
    )
  }
)

parse_family_links <- function(rec_lines, as_spouse = TRUE){
  if(as_spouse) tag <- "FAMS" else tag <- "FAMC"
  link_lst <- find_ged_values(rec_lines, tag, return_list = TRUE) 

  lapply(link_lst, \(x){

    if(tag == "FAMC"){
      lnk <- FamilyLinkChild(
        fam_xref = find_ged_values(x, tag),
        pedigree = find_ged_values(x, c(tag,"PEDI")),
        pedigree_phrase = find_ged_values(x, c(tag,"PEDI","PHRASE")),
        confidence = find_ged_values(x, c(tag,"STAT")),
        confidence_phrase = find_ged_values(x, c(tag,"STAT","PHRASE"))
      )
    } else {
      lnk <- FamilyLinkSpouse(
        fam_xref = find_ged_values(x, tag)
      )
    }
    
    lnk@note_xrefs <- find_ged_values(x, c(tag,"SNOTE"))
    lnk@notes <- parse_notes(x, tag)
    lnk
  })
}


S7::method(summary, FamilyLinkSpouse) <- function(object, ...){
  exdent <- 15
  chil_link <- S7::S7_inherits(object, FamilyLinkChild)
  if(chil_link) link_type <- "as child" else link_type <- "as spouse"
  to_console_value_with_phrase("Family XREF:", 
                               object@fam_xref, link_type, 
                               exdent)
  
  if(chil_link){
    to_console_value_with_phrase("Pedigree:", 
                                 object@pedigree, object@pedigree_phrase, 
                                 exdent)
    to_console_value_with_phrase("Confidence:", 
                                 object@confidence, object@confidence_phrase, 
                                 exdent)
  }
  cat("\n")
  to_console("Notes:", length(object@notes) + length(object@note_xrefs), exdent)
}

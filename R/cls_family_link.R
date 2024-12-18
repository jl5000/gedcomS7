
#' Create a family link (as spouse) object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM family link as a spouse.
#' @export
#' @tests
#' expect_error(FamilyLinkSpouse(), regexp = "@fam_xref has too few elements")
#' expect_snapshot_value(FamilyLinkSpouse("@F123@")@c_as_ged, "json2")
#' expect_snapshot_value(FamilyLinkSpouse("@F2@", 
#'                                                notes = list(Note("test")))@c_as_ged, "json2")
FamilyLinkSpouse <- S7::new_class(
  "FamilyLinkSpouse",
  properties = list(
    fam_xref = S7::new_property(S7::class_character,
                                validator = function(value){
                                  c(
                                    chk_input_size(value, 1, 1),
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  )
                                }),
    note_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    notes = S7::new_property(S7::class_list | 
                               S7::new_S3_class("gedcomS7::Note") | 
                               S7::class_character,
                             validator = function(value){
                               chk_input_S7classes(value, Note, ".+")
                             }),
    
    c_as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 FAMS %s", self@fam_xref),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_xrefs)
        )
      })
  )
)

#' Create a family link (as child) object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM family link as a child.
#' @export
#' @tests
#' expect_error(FamilyLinkChild("@F123@", pedigree = "father"), 
#'                                      regexp = "@pedigree has an invalid value")
#' expect_error(FamilyLinkChild("@F123@", pedigree = "OTHER"), 
#'                                      regexp = "An OTHER pedigree requires explanation in @pedigree_phrase")
#' expect_error(FamilyLinkChild("@F123@", confidence = "LOW"), 
#'                                      regexp = "@confidence has an invalid value")  
#' expect_error(FamilyLinkChild("@F123@", confidence_phrase = "Don't know"), 
#'                                      regexp = "@confidence_phrase requires a @confidence")                                  
#' expect_snapshot_value(FamilyLinkChild("@F2@", 
#'                                                pedigree = "ADOPTED",
#'                                                pedigree_phrase = "By people",
#'                                                confidence = "CHALLENGED",
#'                                                confidence_phrase = "By someone",
#'                                                note_xrefs = c("@242@","@GJFJ@"))@c_as_ged, "json2")
FamilyLinkChild <- S7::new_class(
  "FamilyLinkChild", 
  parent = FamilyLinkSpouse,
  properties = list(
    pedigree = S7::new_property(S7::class_character,
                                validator = function(value){
                                  c(
                                    chk_input_size(value, 0, 1),
                                    chk_input_choice(value, val_pedigree_types())
                                  )
                                }),
    pedigree_phrase = S7::new_property(S7::class_character,
                                       validator = function(value){
                                         chk_input_size(value, 0, 1, 1)
                                       }),
    confidence = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    c(
                                      chk_input_size(value, 0, 1),
                                      chk_input_choice(value, val_confidence_types())
                                    )
                                  }),
    confidence_phrase = S7::new_property(S7::class_character,
                                         validator = function(value){
                                           chk_input_size(value, 0, 1, 1)
                                         }),
    
    c_as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 FAMC %s", self@fam_xref),
          sprintf("1 PEDI %s", self@pedigree),
          sprintf("2 PHRASE %s", self@pedigree_phrase),
          sprintf("1 STAT %s", self@confidence),
          sprintf("2 PHRASE %s", self@confidence_phrase),
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    err <- NULL
    if(chronify(self@pedigree) == "OTHER" && length(self@pedigree_phrase) == 0)
      err <- "An OTHER pedigree requires explanation in @pedigree_phrase."
    c(
      err,
      chk_input_parents(self@pedigree_phrase, "@pedigree_phrase", self@pedigree, "@pedigree"),
      chk_input_parents(self@confidence_phrase, "@confidence_phrase", self@confidence, "@confidence")
    )
  }
)

parse_family_links <- function(rec_lines, as_spouse = TRUE){
  if(as_spouse) tag <- "FAMS" else tag <- "FAMC"
  link_lst <- find_ged_values(rec_lines, tag, return_list = TRUE) 
  if(length(link_lst) == 0) return(list())
  
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

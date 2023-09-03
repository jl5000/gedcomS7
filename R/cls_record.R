#' @include cls_validators.R
NULL

#' Create a base record object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object containing common elements of a GEDCOM record.
#' @include cls_note.R cls_citation.R cls_media_link.R cls_change_date.R
#' @tests
#' expect_error(class_record(), regexp = "@xref has too few elements")
#' expect_error(class_record("REF"), regexp = "@xref is in an invalid format")
#' expect_error(class_record("@1@", unique_ids = letters), regexp = "@unique_ids is in an invalid format")
#' expect_error(class_record("@1@", ext_ids = LETTERS), regexp = "@ext_ids names has too few elements")
#' expect_snapshot_value(class_record("@1@",
#'                                    unique_ids = "a95b5007-2ad2-4bac-81b0-7184243c4512",
#'                                    ext_ids = setNames(letters, LETTERS)[1:5],
#'                                    user_ids = month.abb[1:6])@ids, "json2")
class_record <- S7::new_class(
  "class_record", #abstract = TRUE,
  properties = list(
    xref = S7::class_character,
    confidential = S7::new_property(S7::class_logical, default = FALSE),
    locked = S7::new_property(S7::class_logical, default = FALSE),
    private = S7::new_property(S7::class_logical, default = FALSE),
    user_ids = S7::class_character, # potentially named
    unique_ids = S7::class_character, # not named
    ext_ids = S7::class_character, # definitely named
    note_xrefs = S7::class_character,
    notes = S7::class_list | class_note | S7::class_character,
    citations = S7::class_list | class_citation | S7::class_character,
    media_links = S7::class_list | class_media_link | S7::class_character,
    created = NULL | class_creation_date,
    updated = NULL | class_change_date,
    
    restrictions = S7::new_property(S7::class_character,
                                    getter = function(self){
                                      if(sum(self@confidential, self@locked, self@private) == 0)
                                        return(character())
                                      
                                      conf <- rep("CONFIDENTIAL", self@confidential)
                                      lock <- rep("LOCKED", self@locked)
                                      priv <- rep("PRIVACY", self@private)
                                      
                                      toString(c(conf, lock, priv))
                                    }),
    
    ids = S7::new_property(S7::class_character,
                           getter = function(self){
                             c(
                               named_vec_to_ged(self@user_ids, "REFN", "TYPE"),
                               sprintf("0 UID %s", self@unique_ids),
                               named_vec_to_ged(self@ext_ids, "EXID", "TYPE")
                             )
                           })
  ),
  validator = function(self){
    c(
      chk_input_size(self@xref, "@xref", 1, 1),
      chk_input_pattern(self@xref, "@xref", reg_xref(TRUE)),
      chk_input_size(self@confidential, "@confidential", 1, 1),
      chk_input_size(self@locked, "@locked", 1, 1),
      chk_input_size(self@private, "@private", 1, 1),
      chk_input_size(self@user_ids, "@user_ids", min_val = 1),
      chk_input_pattern(self@unique_ids, "@unique_ids", reg_uuid(TRUE)),
      chk_input_size(self@ext_ids, "@ext_ids", min_val = 1),
      chk_input_size(names(self@ext_ids), "@ext_ids names", length(self@ext_ids), length(self@ext_ids)),
      chk_input_pattern(self@note_xrefs, "@note_xrefs", reg_xref(TRUE)),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+"),
      chk_input_S7classes(self@citations, "@citations", class_citation, reg_xref(TRUE)),
      chk_input_S7classes(self@media_links, "@media_links", class_media_link, reg_xref(TRUE)),
      chk_input_size(self@created, "@created", 0, 1),
      chk_input_pattern(self@created, "@created", reg_date_exact(TRUE)),
      chk_input_size(self@updated, "@updated", 0, 1),
      chk_input_pattern(self@updated, "@updated", reg_date_exact(TRUE))
    )
  }
)

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
  
  rec_lines <- c(x@indi, x@famg, x@sour, x@repo,
                  x@media, x@note, x@subm)[[xref]]
  
  rec_type <- extract_ged_tag(rec_lines[1])
  if(!rec_type %in% c("INDI","FAM","SOUR","REPO","SNOTE","OBJE","SUBM"))
    stop("Record type not recognised: ", rec_type)
  
  rec_xref <- extract_ged_xref(rec_lines[1])
  
  if(rec_type == "INDI"){
    
    rec <- class_record_indi(
      xref = rec_xref,
      sex = toupper(find_ged_values(rec_lines, "SEX")),
      personal_names = extract_personal_names(rec_lines),
      facts = extract_facts_indi(rec_lines),
      family_links = extract_family_links(rec_lines),
      associations = extract_associations(rec_lines)
    )
    
  } else if(rec_type == "FAM"){
    
    chil_xref <- find_ged_values(rec_lines, "CHIL")
    biol_xref <- adop_xref <- fost_xref <- character()
    for(chil in chil_xref){
      chil_lines <- x@indi[[chil]]
      links <- extract_family_links(chil_lines)
      
      for(lnk in links){
        if(lnk@xref == rec_xref){
          if(is_adop_child_link(lnk)){
            adop_xref <- c(adop_xref, chil)
          } else if(is_fost_child_link(lnk)){
            fost_xref <- c(fost_xref, chil)
          } else if(is_birth_child_link(lnk)){
            biol_xref <- c(biol_xref, chil)
          }
        }
        
      }
    }
    
    rec <- class_record_fam(
      xref = rec_xref,
      husb_xref = find_ged_values(rec_lines, "HUSB"),
      wife_xref = find_ged_values(rec_lines, "WIFE"),
      chil_biol_xref = biol_xref,
      chil_adop_xref = adop_xref,
      chil_fost_xref = fost_xref,
      facts = extract_facts_famg(rec_lines),
      num_children = as.integer(find_ged_values(rec_lines, "NCHI"))
    )
    
  } else if(rec_type == "SOUR"){
    
    data_nts <- find_ged_values(rec_lines, c("DATA","NOTE"))
    
    rec <- class_record_sour(
      xref = rec_xref,
      full_title = find_ged_values(rec_lines, "TITL"),
      facts_recorded = extract_events_recorded(rec_lines),
      responsible_agency = find_ged_values(rec_lines, c("DATA","AGNC")),
      data_note_links = data_nts[grepl(reg_xref(TRUE), data_nts)],
      data_notes = data_nts[!grepl(reg_xref(TRUE), data_nts)],
      originator = find_ged_values(rec_lines, "AUTH"),
      short_title = find_ged_values(rec_lines, "ABBR"),
      publication_facts = find_ged_values(rec_lines, "PUBL"),
      source_text = find_ged_values(rec_lines, "TEXT"),
      repo_citations = extract_repo_citations(rec_lines)
    )
    
  } else if(rec_type == "REPO"){
    
    rec <- class_record_repo(
      xref = rec_xref,
      repo_name = find_ged_values(rec_lines, "NAME"),
      address = extract_address(rec_lines),
      phone_numbers = find_ged_values(rec_lines, "PHON"),
      emails = find_ged_values(rec_lines, "EMAIL"),
      faxes = find_ged_values(rec_lines, "FAX"),
      web_pages = find_ged_values(rec_lines, "WWW")
    )
    
  } else if(rec_type == "OBJE"){
    
    rec <- class_record_media(
      xref = rec_xref,
      files = extract_media_files(rec_lines)
    )
    
  } else if(rec_type == "SNOTE"){
    
    rec <- class_record_note(
      xref = rec_xref,
      text = extract_ged_value(rec_lines[1]),
      media_type = find_ged_values(rec_lines, c("SNOTE","MIME")),
      language = find_ged_values(rec_lines, c("SNOTE","LANG")),
      translations = extract_translations(rec_lines)
    )
    
  } else if(rec_type == "SUBM"){
    
    rec <- class_record_subm(
      xref = rec_xref,
      subm_name = find_ged_values(rec_lines, "NAME"),
      address = extract_address(rec_lines),
      phone_numbers = find_ged_values(rec_lines, "PHON"),
      emails = find_ged_values(rec_lines, "EMAIL"),
      faxes = find_ged_values(rec_lines, "FAX"),
      web_pages = find_ged_values(rec_lines, "WWW"),
      languages = find_ged_values(rec_lines, "LANG")
    )
    
  }
  
  resn <- find_ged_values(rec_lines, "RESN")
  rec@locked <- grepl("LOCKED", resn)
  rec@confidential <- grepl("CONFIDENTIAL", resn)
  rec@private <- grepl("PRIVATE", resn)
  rec@user_ids <- extract_vals_and_types(rec_lines, "REFN")
  rec@ext_ids <- extract_vals_and_types(rec_lines, "EXID")
  rec@unique_ids <- find_ged_values(rec_lines, "UID")
  rec@note_xrefs <- find_ged_values(rec_lines, "SNOTE")
  rec@notes <- extract_notes(rec_lines)
  rec@media_links <- extract_media_links(rec_lines)
  rec@citations <- extract_citations(rec_lines)
  rec@updated <- extract_change_date(rec_lines) 
  rec@created <- extract_creation_date(rec_lines)
  
  rec
}
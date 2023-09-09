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


#' Parse common elements into a record object
#'
#' @param rec An S7 record object.
#' @param lines A character vector of lines of a GEDCOM record.
#'
#' @return The S7 record object with common elements added as properties.
extract_common_record_elements <- function(rec, rec_lines){
  
  resn <- find_ged_values(rec_lines, "RESN")
  if(length(resn) > 0){
    rec@locked <- grepl("LOCKED", resn)
    rec@confidential <- grepl("CONFIDENTIAL", resn)
    rec@private <- grepl("PRIVATE", resn)
  }
  
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
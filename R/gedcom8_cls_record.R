

Record_G8 <- S7::new_class(
  "Record_G8", 
  parent = GedcomS7class,
  abstract = TRUE,
  properties = list(
    XREF = prop_char(1, 1, pattern = reg_xref(TRUE), default = new_xref()),
    confidential = prop_bool(default = FALSE),
    locked = prop_bool(default = FALSE),
    private = prop_bool(default = FALSE),
    user_ids = prop_char(min_char = 1), # potentially named
    unique_ids = prop_char(pattern = reg_uuid(TRUE)), # not named
    ext_ids = prop_char(min_char = 1, names_required = TRUE), # definitely named
    # REPLACE WITH NEW G8 NOTE AND CITA structures
    # note_xrefs = prop_char(pattern = reg_xref(TRUE)),
    # notes = prop_S7list("notes", Note),
    # citations = prop_S7list("citations", SourceCitation),
    media_links = prop_S7list("media_links", MediaLink),
    created = prop_S7obj("created", CreationDate),
    updated = prop_S7obj("updated", ChangeDate)
  )
)


#' Parse common elements into a record object
#'
#' @param rec An S7 record object.
#' @param rec_lines A character vector of lines of a GEDCOM record.
#'
#' @returns The S7 record object with common elements added as properties.
#' @keywords internal
parse_common_record_elements <- function(rec, rec_lines){
  
  S7::props(rec) <- list(
    user_ids = parse_vals_and_types(rec_lines, "REFN"),
    ext_ids = parse_vals_and_types(rec_lines, "EXID"),
    unique_ids = find_ged_values(rec_lines, "UID"),
    # note_xrefs = find_ged_values(rec_lines, "SNOTE"),
    # notes = parse_notes(rec_lines),
    media_links = parse_media_links(rec_lines),
  #  citations = parse_citations(rec_lines),
    updated = parse_change_date(rec_lines),
    created = parse_creation_date(rec_lines)
  )
  
  resn <- find_ged_values(rec_lines, "RESN")
  if(length(resn) > 0){
    S7::props(rec) <- list(
      locked = grepl("LOCKED", resn, fixed = TRUE),
      confidential = grepl("CONFIDENTIAL", resn, fixed = TRUE),
      private = grepl("PRIVACY", resn, fixed = TRUE)
    )
  }
  
  rec
  
}



#' @tests
#' maximal <- test_path("maximal70.ged")
#' maximal <- withr::local_tempfile(lines = fix_maximal_records(maximal), 
#'                                  fileext = ".ged")
#' ged <- read_gedcom(maximal)
#' 
#' for(rec_type in names(ged@records@prefixes)){
#'   xrefs <- ged@records@XREFS[[rec_type]]
#'   
#'   for(xref in xrefs){
#'     rec_raw <- S7::prop(ged@records@RAW, rec_type)[[xref]]
#'     
#'     # Remove extension tags
#'     rec_raw <- rec_raw[grepl(anchor_it(reg_tag()), parse_line_tag(rec_raw))]
#'     rec_parsed <- suppressWarnings(pull_record(ged, xref))
#'     
#'     expect_equal(rec_parsed@GEDCOM, rec_raw)
#'   }
#' }
Record <- S7::new_class(
  "Record", 
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
    note_xrefs = prop_char(pattern = reg_xref(TRUE)),
    notes = prop_S7list("notes", Note),
    citations = prop_S7list("citations", SourceCitation),
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
    note_xrefs = find_ged_values(rec_lines, "SNOTE"),
    notes = parse_notes(rec_lines),
    media_links = parse_media_links(rec_lines),
    citations = parse_citations(rec_lines),
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

print_record_summary <- function(x){
  exdent <- 15
  to_console("User IDs:", toString(paste(names(x@user_ids), x@user_ids, sep = " = ")), exdent)
  to_console("Unique IDs:", toString(x@unique_ids), exdent)
  to_console("External IDs:", toString(paste(names(x@ext_ids), x@ext_ids, sep = "/")), exdent)
  to_console("Restrictions:", restrictions_to_resn(x@confidential, x@locked, x@private), exdent)
  if(!is.null(x@created)) summary(x@created)
  if(!is.null(x@updated)) summary(x@updated)
}

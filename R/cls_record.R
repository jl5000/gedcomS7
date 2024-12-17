
#' Create a base record object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object containing common elements of a GEDCOM record.
#' @tests
#' skip_if_offline(host = "gedcom.io")
#' ged_raw <- readLines("https://gedcom.io/testfiles/gedcom70/maximal70.ged")
#' ged_parsed <- read_gedcom("https://gedcom.io/testfiles/gedcom70/maximal70.ged")
#' 
#' for(rec_type in names(ged_parsed@xref_prefixes)){
#'   xrefs <- ged_parsed@c_xrefs[[rec_type]]
#'   
#'   for(xref in xrefs){
#'     rec_raw <- S7::prop(ged_parsed, rec_type)[[xref]]
#'     # Remove extension tags
#'     rec_raw <- rec_raw[grepl(anchor_it(reg_tag()), parse_line_tag(rec_raw))]
#'     rec_parsed <- suppressWarnings(pull_record(ged_parsed, xref))
#'     
#'     expect_equal(rec_parsed@c_as_ged, rec_raw)
#'   }
#' }
class_record <- S7::new_class(
  "class_record", 
  abstract = TRUE,
  properties = list(
    xref = S7::new_property(S7::class_character, default = "@ORPHAN@",
                            validator = function(value){
                              c(
                                chk_input_size(value, 1, 1),
                                chk_input_pattern(value, reg_xref(TRUE))
                              )
                            }),
    confidential = S7::new_property(S7::class_logical, default = FALSE,
                                    validator = function(value){
                                      chk_input_size(value, 1, 1)
                                    }),
    locked = S7::new_property(S7::class_logical, default = FALSE,
                              validator = function(value){
                                chk_input_size(value, 1, 1)
                              }),
    private = S7::new_property(S7::class_logical, default = FALSE,
                               validator = function(value){
                                 chk_input_size(value, 1, 1)
                               }),
    user_ids = S7::new_property(S7::class_character, # potentially named
                                validator = function(value){
                                  chk_input_size(value, min_val = 1)
                                }), 
    unique_ids = S7::new_property(S7::class_character, # not named
                                  validator = function(value){
                                    chk_input_pattern(value, reg_uuid(TRUE))
                                  }), 
    ext_ids = S7::new_property(S7::class_character, # definitely named
                               validator = function(value){
                                 c(
                                   chk_input_size(value, min_val = 1),
                                   chk_input_size(names(value), length(value), length(value), 1)
                                 )
                               }), 
    note_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    notes = S7::new_property(S7::class_list | 
                               S7::new_S3_class("gedcomS7::class_note") | 
                               S7::class_character,
                             validator = function(value){
                               chk_input_S7classes(value, class_note, ".+")
                             }),
    citations = S7::new_property(S7::class_list | 
                                   S7::new_S3_class("gedcomS7::class_citation") | 
                                   S7::class_character,
                                 validator = function(value){
                                   chk_input_S7classes(value, class_citation, reg_xref(TRUE))
                                 }),
    media_links = S7::new_property(S7::class_list | 
                                     S7::new_S3_class("gedcomS7::class_media_link") | 
                                     S7::class_character,
                                   validator = function(value){
                                     chk_input_S7classes(value, class_media_link, reg_xref(TRUE))
                                   }),
    created = S7::new_property(NULL | S7::new_S3_class("gedcomS7::class_creation_date"),
                               validator = function(value){
                                 chk_input_size(value, 0, 1)
                               }),
    updated = S7::new_property(NULL | S7::new_S3_class("gedcomS7::class_change_date"),
                               validator = function(value){
                                 chk_input_size(value, 0, 1)
                               }),
    
    c_restrictions = S7::new_property(S7::class_character,
                                    getter = function(self){
                                      restrictions_to_resn(self@confidential, self@locked, self@private)
                                    }),
    
    c_ids_as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          named_vec_to_ged(self@user_ids, "REFN", "TYPE"),
          sprintf("0 UID %s", self@unique_ids),
          named_vec_to_ged(self@ext_ids, "EXID", "TYPE")
        )
      })
  )
)


#' Parse common elements into a record object
#'
#' @param rec An S7 record object.
#' @param rec_lines A character vector of lines of a GEDCOM record.
#'
#' @return The S7 record object with common elements added as properties.
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
      locked = grepl("LOCKED", resn),
      confidential = grepl("CONFIDENTIAL", resn),
      private = grepl("PRIVACY", resn)
    )
  }
  
  rec
  
}

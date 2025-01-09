
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
    xref = S7::new_property(S7::class_character, default = "@GEDCOMS7_ORPHAN@",
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
    notes = S7::new_property(S7::class_list,
                             getter = function(self) self@notes,
                             setter = function(self, value){
                               self@notes <- as.S7class_list(value, gedcomS7::Note)
                               self
                             },
                             validator = function(value){
                               for(inp in value) if(is.character(inp)) return(inp)
                             }),
    citations = S7::new_property(S7::class_list,
                                 getter = function(self) self@citations,
                                 setter = function(self, value){
                                   self@citations <- as.S7class_list(value, gedcomS7::SourceCitation)
                                   self
                                 },
                                 validator = function(value){
                                   for(inp in value) if(is.character(inp)) return(inp)
                                 }),
    media_links = S7::new_property(S7::class_list,
                                   getter = function(self) self@media_links,
                                   setter = function(self, value){
                                     self@media_links <- as.S7class_list(value, gedcomS7::MediaLink)
                                     self
                                   },
                                   validator = function(value){
                                     for(inp in value) if(is.character(inp)) return(inp)
                                   }),
    created = S7::new_property(NULL | S7::new_S3_class("gedcomS7::CreationDate"),
                               validator = function(value){
                                 chk_input_size(value, 0, 1)
                               }),
    updated = S7::new_property(NULL | S7::new_S3_class("gedcomS7::ChangeDate"),
                               validator = function(value){
                                 chk_input_size(value, 0, 1)
                               }),
    
    RESTRICTIONS = S7::new_property(S7::class_character,
                                    getter = function(self){
                                      restrictions_to_resn(self@confidential, self@locked, self@private)
                                    }),
    
    GEDCOM_IDENTIFIERS = S7::new_property(
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
      locked = grepl("LOCKED", resn),
      confidential = grepl("CONFIDENTIAL", resn),
      private = grepl("PRIVACY", resn)
    )
  }
  
  rec
  
}


# S7::method(print, Record) <- function(x, ...){
#   summary(x)
# }
# 
# print_record_summary <- function(x){
#   exdent <- 24
#   to_console("Restrictions:", x@RESTRICTIONS, exdent)
#   
#   
#   
# }

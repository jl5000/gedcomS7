
#' Create a source citation object
#' 
#' @inheritParams prop_definitions
#' @param sour_xref The cross-reference identifier of a source record. If the source
#' does not have a record, then this can be left blank and the value "@VOID@" will be used. However,
#' you should describe the source in @where.
#' @param where A specific location within the information referenced. For a published work, this could
#' include the volume of a multi-volume work and the page number or numbers. For a
#' periodical, it could include volume, issue, and page numbers. For a newspaper, it could
#' include a date, page number, and column number. For an unpublished source or micro‐
#' filmed works, this could be a film or sheet number, page number, or frame number. A
#' census record might have an enumerating district, page number, line number, dwelling
#' number, and family number.
#' It is recommended that the data in this field be formatted comma-separated with label:
#'   value pairs
#' @param fact_phrase Textual information that cannot be expressed in the fact type.
#' @param role What role this person played in this fact.
#' @param role_phrase Textual information that cannot be expressed in the role.
#' @param certainty An enumerated value indicating the credibility of a
#' piece of information, based on its supporting evidence. Some systems use this feature to
#' rank multiple conflicting opinions for display of most likely information first. It is not
#' intended to eliminate the receivers’ need to evaluate the evidence for themselves.
#' "0" = unreliable/estimated data
#' "1" = Questionable reliability of evidence 
#' "2" = Secondary evidence, data officially recorded sometime after event
#' "3" = Direct and primary evidence used, or by dominance of the evidence
#' 
#' @returns An S7 object representing a GEDCOM SOURCE_CITATION.
#' @export
#' @tests
#' expect_snapshot_value(SourceCitation()@GEDCOM, "json2")
#' expect_error(SourceCitation("@1@", date = ""),
#'              regexp = "A blank @date requires a @date_phrase")
#' expect_error(SourceCitation("@1@", fact_phrase = "phrase"),
#'              regexp = "@fact_phrase requires a @fact_type")
#' expect_error(SourceCitation("@1@", role = "HUSB"),
#'              regexp = "@role requires a @fact_type")
#' expect_error(SourceCitation("@1@", fact_type = "BIRT", role_phrase = "phrase"),
#'              regexp = "@role_phrase requires a @role")
#' expect_error(SourceCitation("@1@", fact_type = "BIRT", role = "OTHER"),
#'              regexp = "A @role_phrase must be given if @role is 'OTHER'")
#' expect_error(SourceCitation("@1@", certainty = "4"),
#'              regexp = "@certainty has an invalid value")
#' expect_error(SourceCitation("@1@", notes = ""),
#'              regexp = "@text has too few characters")
#' expect_error(SourceCitation("@1@", fact_type = "birth"),
#'              regexp = "@fact_type has an invalid value")             
#' expect_snapshot_value(SourceCitation("@1@",
#'                                      where = "page 2",
#'                                      date = "2 JUN 2006",
#'                                      source_text = c("verbatim","text"),
#'                                      fact_type = "BIRT",
#'                                      fact_phrase = "Parish births",
#'                                      role = "HUSB",
#'                                      role_phrase = "phrase",
#'                                      certainty = 3,
#'                                      media_links = MediaLink("@34E@"),
#'                                      note_xrefs = c("@WER@",
#'                                                    "@4334@"),
#'                                      notes = c("these are","some notes"))@GEDCOM, "json2")
SourceCitation <- S7::new_class(
  "SourceCitation",
  parent = GedcomS7class,
  properties = list(
    sour_xref = prop_char(1, 1, pattern = reg_xref(TRUE), default = "@VOID@"),
    where = prop_char(0, 1, 1),
    date = prop_char(0, 1, pattern = reg_date_value(), S7class_names = "DateValue"),
    source_text = prop_S7list("source_text", TranslationText),
    fact_type = prop_char(0, 1, choices = val_fact_types()),
    fact_phrase = prop_char(0, 1, 1),
    role = prop_char(0, 1, choices = val_roles()),
    role_phrase = prop_char(0, 1, 1),
    certainty = prop_char(0, 1, choices = val_certainty(), casting_name = "certainty"),
    media_links = prop_S7list("media_links", MediaLink),
    note_xrefs = prop_char(pattern = reg_xref(TRUE)),
    notes = prop_S7list("notes", Note),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 SOUR %s", self@sour_xref),
          sprintf("1 PAGE %s", self@where),
          rep("1 DATA", length(self@date) + 
                length(self@source_text) > 0),
          obj_to_ged(self@date, "DATE") |> increase_level(by = 2),
          obj_to_ged(self@source_text, "TEXT") |> increase_level(by = 2) |> 
            gsub(pattern = "(^\\d) TRAN ", replacement = "\\1 TEXT "),
          sprintf("1 EVEN %s", self@fact_type),
          sprintf("2 PHRASE %s", self@fact_phrase),
          sprintf("2 ROLE %s", self@role),
          sprintf("3 PHRASE %s", self@role_phrase),
          sprintf("1 QUAY %s", self@certainty),
          obj_to_ged(self@media_links, "OBJE") |> increase_level(by = 1),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_xrefs)
        ) 
      })
  ),
  
  validator = function(self){
    errs <- NULL
    if(is.character(self@date) && isTRUE(self@date == ""))
      errs <- c(errs, "A blank @date requires a @date_phrase and therefore requires a DateValue object.")
    
    c(
      errs,
      chk_input_parents(self@fact_phrase, "@fact_phrase", self@fact_type, "@fact_type"),
      chk_input_parents(self@role, "@role", self@fact_type, "@fact_type"),
      chk_input_parents(self@role_phrase, "@role_phrase", self@role, "@role"),
      chk_input_phrase(self@role_phrase, "@role_phrase",
                       self@role, "@role", "OTHER")
    )
  }
  
)

parse_citations <- function(lines, location = NULL){
  
  sour_lst <- find_ged_values(lines, c(location, "SOUR"), return_list = TRUE)
  if(length(sour_lst) == 0) return(list())
  
  lapply(sour_lst, \(x){
    SourceCitation(
      sour_xref = find_ged_values(x, "SOUR"),
      where = find_ged_values(x, c("SOUR","PAGE")),
      date = parse_date_value(x, c("SOUR","DATA")),
      source_text = parse_translations(x, c("SOUR","DATA")),
      fact_type = find_ged_values(x, c("SOUR","EVEN")),
      fact_phrase = find_ged_values(x, c("SOUR","EVEN","PHRASE")),
      role = find_ged_values(x, c("SOUR","EVEN","ROLE")),
      role_phrase = find_ged_values(x, c("SOUR","EVEN","ROLE","PHRASE")),
      certainty = find_ged_values(x, c("SOUR","QUAY")),
      media_links = parse_media_links(x, "SOUR"),
      notes = parse_notes(x, "SOUR"),
      note_xrefs = find_ged_values(x, c("SOUR","SNOTE"))
    )
  })
  
}


S7::method(summary, SourceCitation) <- function(object, ...){
  exdent <- 15
  to_console("Source XREF:", object@sour_xref, exdent)
  to_console("Where:", object@where, exdent)
  to_console("Date:", obj_to_val(object@date), exdent)
  fact_type <- object@fact_type
  fact_type <- names(val_fact_types(TRUE))[fact_type == val_fact_types(TRUE)]
  to_console_value_with_phrase("Fact:", 
                               fact_type, object@fact_phrase, 
                               exdent)
  to_console_value_with_phrase("Role:", 
                               object@role, object@role_phrase, 
                               exdent)
  cat("\n")
  to_console("Media Links:", length(object@media_links), exdent)
  to_console("Notes:", length(object@notes) + length(object@note_xrefs), exdent)
}

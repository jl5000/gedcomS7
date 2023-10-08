
#' Create a repository citation object
#' 
#' @inheritParams prop_definitions
#' @return An S7 object representing a GEDCOM SOURCE_REPOSITORY_CITATION.
#' @export
#' @include cls_note.R
#' @tests
#' expect_snapshot_value(class_repository_citation()@as_ged, "json2")
#' expect_snapshot_value(class_repository_citation(notes = "Local library",
#'                                                 call_numbers = c("ABC","123"))@as_ged, "json2")
class_repository_citation <- S7::new_class(
  "class_repository_citation",
  package = "gedcomS7",
  properties = list(
    repo_xref = S7::new_property(S7::class_character, default = "@VOID@",
                                 validator = function(value){
                                   c(
                                     chk_input_size(value, 1, 1),
                                     chk_input_pattern(value, reg_xref(TRUE))
                                   )
                                 }),
    notes = S7::new_property(S7::class_list | class_note | S7::class_character,
                             validator = function(value){
                               chk_input_S7classes(value, class_note, ".+")
                             }),
    note_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    call_numbers = S7::new_property(S7::class_character,
                                    validator = function(value){
                                      chk_input_size(value, min_val = 1)
                                    }),
    # TODO: too much nesting
    # media = S7::class_character, 
    # media_phrase = S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 REPO %s", self@repo_xref),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_xrefs),
          sprintf("1 CALN %s", self@call_numbers)
          # sprintf("2 MEDI %s", self@media),
          # sprintf("3 PHRASE %s", self@media_phrase)
        )
      })
  )
)

extract_repo_citations <- function(rec_lines){
  repo_lst <- find_ged_values(rec_lines, "REPO", return_list = TRUE) 
  if(length(repo_lst) == 0) return(list())
  
  lapply(repo_lst, \(x){
    class_repository_citation(
      repo_xref = find_ged_values(x, "REPO"),
      note_xrefs = find_ged_values(rec_lines, "SNOTE"),
      notes = extract_notes(rec_lines),
      call_numbers = find_ged_values(x, c("REPO","CALN"))
    )
  })
}

#' Create an object recording facts covered in a source record
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM SOUR.EVEN structure.
#' @export
#' @include cls_date.R cls_place.R
#' @tests
#' expect_error(class_facts_recorded("birth"), regexp = "@fact_types is in an invalid format")
#' expect_error(class_facts_recorded("BIRT "), regexp = "@fact_types is in an invalid format")
#' expect_error(class_facts_recorded("BIRT,DEAT"), regexp = "@fact_types is in an invalid format")
#' expect_error(class_facts_recorded("BIRT, DEAT", date_period = "2006"), 
#'                                   regexp = "@date_period is in an invalid format")
#' expect_snapshot_value(class_facts_recorded("BIRT")@as_ged, "json2")
#' expect_snapshot_value(class_facts_recorded("BIRT, DEAT",
#'                                            date_period = "FROM 2007 TO 2010")@as_ged, "json2")
#' expect_snapshot_value(class_facts_recorded("BIRT, DEAT",
#'                                            date_period = "FROM 2007 TO 2010",
#'                                            date_phrase = "sometime",
#'                                            territory = "somewhere")@as_ged, "json2")
class_facts_recorded <- S7::new_class(
  "class_facts_recorded",
  package = "gedcomS7",
  properties = list(
    fact_types = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    c(
                                      chk_input_size(value, 1, 1),
                                      chk_input_pattern(value, sprintf("^(%s)(, (%s))*$", 
                                                                       paste(val_fact_types(), collapse = "|"),
                                                                       paste(val_fact_types(), collapse = "|")))
                                    )
                                  }),
    date_period = S7::new_property(S7::class_character | class_date_period,
                                   validator = function(value){
                                     c(
                                       chk_input_size(value, 0, 1),
                                       chk_input_pattern(value, reg_date_period())
                                     )
                                   }),
    date_phrase = S7::new_property(S7::class_character,
                                   validator = function(value){
                                     chk_input_size(value, 0, 1, 1)
                                   }),
    territory = S7::new_property(S7::class_character | class_place,
                                 validator = function(value){
                                   chk_input_size(value, 0, 1, 1)
                                 }),
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 EVEN %s", self@fact_types),
          sprintf("1 DATE %s", obj_to_val(self@date_period)) |> trimws(),
          sprintf("2 PHRASE %s", self@date_phrase),
          obj_to_ged(self@territory, "PLAC") |> increase_level(by = 1)
        )
      })
  )
)

extract_events_recorded <- function(rec_lines){
  even_lst <- find_ged_values(rec_lines, c("DATA","EVEN"), return_list = TRUE)
  if(length(even_lst) == 0) return(character())
  
  lapply(even_lst, \(x){
    class_facts_recorded(
      fact_types = find_ged_values(x, "EVEN"),
      date_period = find_ged_values(x, c("EVEN","DATE")),
      date_phrase = find_ged_values(x, c("EVEN","DATE","PHRASE")),
      territory = extract_place(x, "EVEN")
    )
  })
  
}


#' Create a source record object
#' 
#' @inheritParams prop_definitions 
#' @param citations Not used.
#' @return An S7 object representing a GEDCOM SOURCE_RECORD.
#' @export
#' @include cls_record.R cls_translation.R
class_record_sour <- S7::new_class(
  "class_record_sour", 
  package = "gedcomS7",
  parent = class_record,
  properties = list(
    facts_recorded = S7::new_property(S7::class_list | class_facts_recorded | S7::class_character,
                                      validator = function(value){
                                        chk_input_S7classes(value, class_facts_recorded,
                                                            sprintf("^(%s)(, (%s))*$", 
                                                                    paste(val_fact_types(), collapse = "|"),
                                                                    paste(val_fact_types(), collapse = "|")))
                                      }),
    agency = S7::new_property(S7::class_character,
                              validator = function(value){
                                chk_input_size(value, 0, 1, 1)
                              }),
    data_note_xrefs = S7::new_property(S7::class_character,
                                       validator = function(value){
                                         chk_input_pattern(value, reg_xref(TRUE))
                                       }),
    data_notes = S7::new_property(S7::class_list | class_note | S7::class_character,
                                  validator = function(value){
                                    chk_input_S7classes(value, class_note, ".+")
                                  }),
    originator = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_size(value, 0, 1, 1)
                                  }),
    full_title = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_size(value, 0, 1, 1)
                                  }),
    short_title = S7::new_property(S7::class_character,
                                   validator = function(value){
                                     chk_input_size(value, 0, 1, 1)
                                   }),
    publication_facts = S7::new_property(S7::class_character,
                                         validator = function(value){
                                           chk_input_size(value, 0, 1, 1)
                                         }),
    # NOTE I've made the cardinality of this {0,M} to match source citation
    source_text = S7::new_property(S7::class_list | class_translation_txt | S7::class_character,
                                   validator = function(value){
                                     chk_input_S7classes(value, class_translation_txt, ".+")
                                   }),
    repo_citations = S7::new_property(S7::class_list | class_repository_citation | S7::class_character,
                                      validator = function(value){
                                        chk_input_S7classes(value, class_repository_citation, reg_xref(TRUE))
                                      }),
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s SOUR", self@xref),
          sprintf("1 RESN %s", self@restrictions),
          rep("1 DATA", length(self@facts_recorded) + length(self@agency) + 
                length(self@data_notes) + length(self@data_note_xrefs) > 0),
          obj_to_ged(self@facts_recorded, "EVEN") |> increase_level(by = 2),
          sprintf("2 AGNC %s", self@agency),
          obj_to_ged(self@data_notes, "NOTE") |> increase_level(by = 2),
          sprintf("2 SNOTE %s", self@data_note_xrefs),
          sprintf("1 AUTH %s", self@originator),
          sprintf("1 TITL %s", self@full_title),
          sprintf("1 ABBR %s", self@short_title),
          sprintf("1 PUBL %s", self@publication_facts),
          obj_to_ged(self@source_text, "TEXT") |> increase_level(by = 1) |> 
            gsub(pattern = "(^\\d) TRAN ", replacement = "\\1 TEXT "),
          obj_to_ged(self@repo_citations, "REPO") |> increase_level(by = 1),
          self@ids |> increase_level(by = 1),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@media_links, "OBJE") |> increase_level(by = 1),
          obj_to_ged(self@updated) |> increase_level(by = 1),
          obj_to_ged(self@created) |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    if(length(self@citations) > 0)
      return("This record does not use @citations")
  })

extract_record_sour <- function(rec_lines){
  
  rec <- class_record_sour(
    xref = extract_ged_xref(rec_lines[1]),
    facts_recorded = extract_events_recorded(rec_lines),
    agency = find_ged_values(rec_lines, c("DATA","AGNC")),
    data_note_xrefs = find_ged_values(rec_lines, c("DATA","SNOTE")),
    data_notes = extract_notes(rec_lines, "DATA"),
    originator = find_ged_values(rec_lines, "AUTH"),
    full_title = find_ged_values(rec_lines, "TITL"),
    short_title = find_ged_values(rec_lines, "ABBR"),
    publication_facts = find_ged_values(rec_lines, "PUBL"),
    source_text = extract_translations(rec_lines),
    repo_citations = extract_repo_citations(rec_lines)
  )
  
  extract_common_record_elements(rec, rec_lines)
}
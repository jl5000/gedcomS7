
#' Create a source call number object
#' 
#' @inheritParams prop_definitions
#' @param call_number The call number.
#' 
#' @returns An S7 object representing the CALN substructure of a GEDCOM 
#' SOURCE_REPOSITORY_CITATION.
#' @export
SourceCallNumber <- S7::new_class(
  "SourceCallNumber",
  parent = GedcomS7class,
  properties = list(
    call_number = prop_char(1, 1, 1),
    medium = prop_char(0, 1, choices = val_medium_types()),
    medium_phrase = prop_char(0, 1, 1),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 CALN %s", self@call_number),
          sprintf("1 MEDI %s", self@medium),
          sprintf("2 PHRASE %s", self@medium_phrase)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_phrase(self@medium_phrase, "@medium_phrase",
                       self@medium, "@medium", "OTHER"),
      chk_input_parents(self@medium_phrase, "@medium_phrase", self@medium, "@medium")
    )
  }
)

parse_call_numbers <- function(lines, location){
  call_lst <- find_ged_values(lines, c(location, "CALN"), return_list = TRUE) 
  if(length(call_lst) == 0) return(list())
  
  lapply(call_lst, \(x){
    SourceCallNumber(
      call_number = find_ged_values(x, "CALN"),
      medium = find_ged_values(x, c("CALN","MEDI")),
      medium_phrase = find_ged_values(x, c("CALN","MEDI","PHRASE"))
    )
  })
}


#' Create a repository citation object
#' 
#' @inheritParams prop_definitions
#' @param repo_xref The cross-reference identifier of a repository record. If the repository
#' does not have a record, then this can be left blank and a void xref will be used. However,
#' you should describe the repository in @notes.
#' @param call_numbers Call number(s) used to file and retrieve items from the repository. 
#' This can either be a `SourceCallNumber` object, a list of them,
#' or a character vector of call numbers.
#' 
#' @returns An S7 object representing a GEDCOM SOURCE_REPOSITORY_CITATION.
#' @export
#' @tests
#' expect_snapshot_value(RepositoryCitation()@GEDCOM, "json2")
#' expect_snapshot_value(RepositoryCitation(notes = "Local library",
#'                                                 call_numbers = c("ABC","123"))@GEDCOM, "json2")
RepositoryCitation <- S7::new_class(
  "RepositoryCitation",
  parent = GedcomS7class,
  properties = list(
    repo_xref = prop_char(1, 1, pattern = reg_xref(TRUE), default = void_xref()),
    notes = prop_S7list("notes", Note),
    note_xrefs = prop_char(pattern = reg_xref(TRUE)),
    call_numbers = prop_S7list("call_numbers", SourceCallNumber),

    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 REPO %s", self@repo_xref),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@call_numbers, "CALN") |> increase_level(by = 1)
        )
      })
  )
)

parse_repo_citations <- function(rec_lines){
  repo_lst <- find_ged_values(rec_lines, "REPO", return_list = TRUE) 
  if(length(repo_lst) == 0) return(list())
  
  lapply(repo_lst, \(x){
    RepositoryCitation(
      repo_xref = find_ged_values(x, "REPO"),
      note_xrefs = find_ged_values(x, c("REPO","SNOTE")),
      notes = parse_notes(x, "REPO"),
      call_numbers = parse_call_numbers(x, "REPO")
    )
  })
}

#' Create an object recording facts covered in a source record
#' 
#' @inheritParams prop_definitions 
#' @param fact_types A character string indicating the types
#' of events that were recorded in a particular source. Each event type is separated by a
#' comma and space. For example, a parish register of births, deaths, and marriages
#' would be BIRT, DEAT, MARR. 
#' @param territory The territory associated with the events covered. This can either be a 
#' `Place()` object or a character string (a comma-separated string of region names, 
#' ordered from smallest to largest).
#' 
#' @returns An S7 object representing a GEDCOM SOUR.EVEN structure.
#' @export
#' @tests
#' expect_error(FactsRecorded("birth"), regexp = "@fact_types is in an invalid format")
#' expect_error(FactsRecorded("BIRT "), regexp = "@fact_types is in an invalid format")
#' expect_error(FactsRecorded("BIRT,DEAT"), regexp = "@fact_types is in an invalid format")
#' expect_error(FactsRecorded("BIRT, DEAT", date_period = "2006"), 
#'                                   regexp = "@date_period is in an invalid format")
#' expect_snapshot_value(FactsRecorded("BIRT")@GEDCOM, "json2")
#' expect_snapshot_value(FactsRecorded("BIRT, DEAT",
#'                                            date_period = "FROM 2007 TO 2010")@GEDCOM, "json2")
#' expect_snapshot_value(FactsRecorded("BIRT, DEAT",
#'                                            date_period = "FROM 2007 TO 2010",
#'                                            date_phrase = "sometime",
#'                                            territory = "somewhere")@GEDCOM, "json2")
FactsRecorded <- S7::new_class(
  "FactsRecorded",
  parent = GedcomS7class,
  properties = list(
    fact_types = prop_char(1, 1, 
                           pattern = sprintf("^(%s)(, (%s))*$", 
                                             paste(val_fact_types(), collapse = "|"),
                                             paste(val_fact_types(), collapse = "|"))),
    date_period = prop_char(0, 1, pattern = reg_date_period(), S7class_names = "DatePeriod"),
    date_phrase = prop_char(0, 1, 1),
    territory = prop_S7obj("territory", Place),
    
    GEDCOM = S7::new_property(
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

parse_events_recorded <- function(rec_lines){
  even_lst <- find_ged_values(rec_lines, c("DATA","EVEN"), return_list = TRUE)
  if(length(even_lst) == 0) return(character())
  
  lapply(even_lst, \(x){
    FactsRecorded(
      fact_types = find_ged_values(x, "EVEN"),
      date_period = find_ged_values(x, c("EVEN","DATE")),
      date_phrase = find_ged_values(x, c("EVEN","DATE","PHRASE")),
      territory = parse_place(x, "EVEN")
    )
  })
  
}


#' Create a source record object
#' 
#' @inheritParams prop_definitions 
#' @param facts_recorded The facts recorded by the source. This can either be a `FactsRecorded` object, 
#' a list of them, or a character vector of comma-delimited fact types. For example, a parish register of 
#' births, deaths, and marriages would be "BIRT, DEAT, MARR". The `val_fact_types()` function gives a
#' list of possible fact types.
#' @param data_note_xrefs A character vector of note record cross-reference identifiers relevant
#' to the source data.
#' @param data_notes Associated notes about the source data. This can either be a `Note` 
#' object, a list of them, or a character vector of notes.
#' @param originator The person, agency, or entity who created the record. For a published work, 
#' this could be the author, compiler, transcriber, abstractor, or editor. For an unpublished 
#' source, this may be an individual, a government agency, church organization, or private organization.
#' @param full_title The full title of the source.
#' @param short_title A shortened name of the source used for sorting, filing, and retrieving records.
#' @param publication_facts When and where the record was created. For published works, this 
#' includes information such as the city of publication, name of the publisher, and year of publication.
#' @param repo_citations Associated repositories. This can either be a `RepositoryCitation` object, 
#' a list of them, or a character vector of XREFs of repository records.
#' @param citations Not used.
#' 
#' @returns An S7 object representing a GEDCOM SOURCE_RECORD.
#' @export
SourceRecord <- S7::new_class(
  "SourceRecord", 
  parent = Record,
  properties = list(
    facts_recorded = prop_S7list("facts_recorded", FactsRecorded),
    agency = prop_char(0, 1, 1),
    data_note_xrefs = prop_char(pattern = reg_xref(TRUE)),
    data_notes = prop_S7list("data_notes", Note),
    originator = prop_char(0, 1, 1),
    full_title = prop_char(0, 1, 1),
    short_title = prop_char(0, 1, 1),
    publication_facts = prop_char(0, 1, 1),
    # NOTE I've made the cardinality of this {0,M} to match source citation
    source_text = prop_S7list("source_text", TranslationText),
    repo_citations = prop_S7list("repo_citations", RepositoryCitation),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s SOUR", self@XREF),
          sprintf("1 RESN %s", self@RESTRICTIONS),
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
          self@GEDCOM_IDENTIFIERS |> increase_level(by = 1),
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

parse_record_sour <- function(rec_lines){
  
  rec <- SourceRecord(
    XREF = parse_line_xref(rec_lines[1]),
    facts_recorded = parse_events_recorded(rec_lines),
    agency = find_ged_values(rec_lines, c("DATA","AGNC")),
    data_note_xrefs = find_ged_values(rec_lines, c("DATA","SNOTE")),
    data_notes = parse_notes(rec_lines, "DATA"),
    originator = find_ged_values(rec_lines, "AUTH"),
    full_title = find_ged_values(rec_lines, "TITL"),
    short_title = find_ged_values(rec_lines, "ABBR"),
    publication_facts = find_ged_values(rec_lines, "PUBL"),
    source_text = parse_translations(rec_lines),
    repo_citations = parse_repo_citations(rec_lines)
  )
  
  parse_common_record_elements(rec, rec_lines)
}

S7::method(summary, SourceCallNumber) <- function(object, ...){
  exdent <- 15
  to_console("Call Number:", object@call_number, exdent)
  to_console_value_with_phrase("Medium:", 
                               object@medium, object@medium_phrase, 
                               exdent)
}

S7::method(summary, RepositoryCitation) <- function(object, ...){
  exdent <- 15
  to_console("Repos. XREF:", toString(object@repo_xref), exdent)
  to_console_list("Call Numbers:", object@call_numbers, exdent, prop = "call_number")
  cat("\n")
  to_console("Notes:", length(object@notes) + length(object@note_xrefs), exdent)
}

S7::method(summary, FactsRecorded) <- function(object, ...){
  exdent <- 15
  to_console("Facts:", toString(object@fact_types), exdent)
  to_console_value_with_phrase("Period:", 
                               object@date_period, object@date_phrase, 
                               exdent)
  
  if(!is.null(object@territory)) 
    to_console("Territory:", object@territory@GEDCOM_STRING, exdent)
}

S7::method(summary, SourceRecord) <- function(object, ...){
  exdent <- 15
  to_console("XREF:", object@XREF, exdent)
  to_console("Full Title:", object@full_title, exdent)
  to_console("Short Title:", object@short_title, exdent)
  to_console("Originator:", object@originator, exdent)
  to_console("Agency:", object@agency, exdent)
  to_console("Ref. Info:", object@publication_facts, exdent)
  
  facts <- NULL
  for(fct in object@facts_recorded){
    facts <- toString(c(facts, fct@fact_types))
  }
  to_console("Facts:", facts, exdent)
  
  cat("\n")
  to_console("Repos. Links:", length(object@repo_citations), exdent)
  to_console("Media Links:", length(object@media_links), exdent)
  to_console("Notes:", length(object@notes) + length(object@note_xrefs), exdent)
  to_console("Data Notes:", length(object@data_notes) + length(object@data_note_xrefs), exdent)
  cat("\n")
  print_record_summary(object)
}

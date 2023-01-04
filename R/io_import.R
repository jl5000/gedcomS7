
#' Import a GEDCOM file
#'
#' Imports a *.ged file and creates a tidyged object.
#'
#' @param filepath The full filepath of the GEDCOM file.
#'
#' @return A tidyged object
#' @export
#'
#' @examples
#' \dontrun{
#' read_gedcom("C:/my_family.ged")
#' }
#' @tests
#' expect_error(read_gedcom("my_family.txt"))
#' expect_snapshot_value(
#'     read_gedcom(system.file("extdata", "555SAMPLE.GED", package = "tidyged.io")), 
#'     "json2")
#' expect_snapshot_value(
#'     read_gedcom(system.file("extdata", "555SAMPLE16BE.GED", package = "tidyged.io")), 
#'     "json2")
#' expect_snapshot_value(
#'     read_gedcom(system.file("extdata", "555SAMPLE16LE.GED", package = "tidyged.io")), 
#'     "json2")
#' expect_snapshot_value(
#'     read_gedcom(system.file("extdata", "MINIMAL555.GED", package = "tidyged.io")), 
#'     "json2")
read_gedcom <- function(filepath = file.choose()) {
  
  if(tolower(substr(filepath, nchar(filepath)-3 , nchar(filepath))) != ".ged")
    stop("GEDCOM file should have a .ged extension")
  
  gedcom_encoding <- read_gedcom_encoding(filepath)
  
  con <- file(filepath, encoding = gedcom_encoding)
  on.exit(close(con))
  
  ged_lines <- readLines(con)
  
  validate_lines(ged_lines)
  
  validate_header(ged_lines[1:6], gedcom_encoding)
  
  ged_lines <- combine_gedcom_values(ged_lines)
  
  records_lst <- split(ged_lines, cumsum(substr(ged_lines, 1, 1) == "0"))
  
  parse_records(records_lst)
  
}


#' Read the Byte Order Mark of the GEDCOM file
#' 
#' This function reads the Byte Order Mark of a GEDCOM file in order to determine its encoding.
#' It only checks for UTF-8 or UTF-16 - if neither of these are found it throws an error.
#'
#' @param filepath The full filepath of the GEDCOM file.
#'
#' @return A character string indicating the encoding of the file.
#' @tests
#' expect_equal(
#'   read_gedcom_encoding(system.file("extdata", "555SAMPLE.GED", package = "tidyged.io")), 
#'   "UTF-8")
#' expect_equal(
#'   read_gedcom_encoding(system.file("extdata", "555SAMPLE16BE.GED", package = "tidyged.io")), 
#'   "UTF-16BE")
#' expect_equal(
#'   read_gedcom_encoding(system.file("extdata", "555SAMPLE16LE.GED", package = "tidyged.io")), 
#'   "UTF-16LE")
read_gedcom_encoding <- function(filepath) {
  
  if(identical(as.character(readBin(filepath, 'raw', 3)), .pkgenv$BOM_UTF8)) {
    return("UTF-8")  
  } else if(identical(as.character(readBin(filepath, 'raw', 2)), .pkgenv$BOM_UTF16_BE)) {
    return("UTF-16BE")
  } else if(identical(as.character(readBin(filepath, 'raw', 2)), .pkgenv$BOM_UTF16_LE)) {
    return("UTF-16LE")
  } else {
    stop("Invalid file encoding. Only UTF-8 and UTF-16 Byte Order Marks are supported")
  }
  
}

validate_lines <- function(lines){
  
  if(any(nchar(lines) > .pkgenv$gedcom_line_length_limit)) 
    stop("This is not a GEDCOM 5.5.5 file. The following lines are too long: ", 
         paste(which(nchar(lines) > .pkgenv$gedcom_line_length_limit), collapse=","))
  
  reg_hd_tr <- "^(0 HEAD)|(0 TRLR)$"
  reg_rec_start <- sprintf("^0 %s (SUBM|INDI|FAM|SOUR|REPO|OBJE)$", reg_xref(FALSE))
  reg_rec_start_note <- sprintf("^0 %s NOTE .{1,%s}$", reg_xref(FALSE), .pkgenv$gedcom_phys_value_limit)
  reg_sub_rec <- sprintf("^[1-6] [A-Z1-3]{3,5}( .{0,%s})?$", .pkgenv$gedcom_phys_value_limit)
  reg_line <- paste(reg_hd_tr, reg_rec_start, reg_rec_start_note, reg_sub_rec, sep = "|")
  
  invalid_lines <- grep(reg_line, lines, invert = TRUE)
  
  if(length(invalid_lines) > 0)
    stop(paste(c("The following lines are invalid:", 
                 paste(invalid_lines, lines[invalid_lines], sep = ": ")), 
               collapse = "\n"))
  
  NULL
}

validate_header <- function(header_lines, expected_encoding) {
  
  expected_header = c(
    "0 HEAD",
    "1 GEDC",
    "2 VERS 5.5.5",
    "2 FORM LINEAGE-LINKED",
    "3 VERS 5.5.5"
  )
  
  if(!isTRUE(all.equal(header_lines[1:5], expected_header)))
    stop("Malformed header")
  
  char <- sub("1 CHAR ", "", header_lines[6])
  
  if(expected_encoding == "UTF-8") {
    if(char != "UTF-8") stop("Character encodings do not match")
  } else if(expected_encoding %in% c("UTF-16BE", "UTF-16LE")) {
    if(char != "UNICODE") stop("Character encodings do not match")
  } else {
    stop("Character encoding not recognised")
  }
  
  NULL
}

#' Convert the GEDCOM grammar to the GEDCOM form
#' 
#' This function applies concatenation indicated by CONC/CONT lines.
#' 
#' The function works by collapsing CONC/CONT lines using group-by/summarise.   
#'
#' @param gedcom A tidyged object.
#'
#' @return A tidyged object in the GEDCOM form.
combine_gedcom_values <- function(lines) {
  
  reg_cont_conc <- sprintf("^[1-6] (CONT|CONC) (.*)$")
  unique_delim <- "<><>unique_delim<><>"
  
  lines <- lines |> 
    gsub(pattern = "\n\r|\r\n", replacement = "\n") |> 
    gsub(pattern = "\r", replacement = "\n") |>
    sub(pattern = "CONT ", replacement = "CONT \n")
  
  # Prepare lines for merging
  cont_conc_lines <- grep(reg_cont_conc, lines)
  lines[cont_conc_lines] <- sub(reg_cont_conc, "\\2",
                                lines[cont_conc_lines])
  lines[-cont_conc_lines] <- paste0(unique_delim,
                                    lines[-cont_conc_lines])
  
  # Merge and seperate again
  lines <- paste(lines, collapse = "") |>
    strsplit(unique_delim) |>
    unlist()
  
  lines[-1] # delete empty line introduced
}


parse_records <- function(records_lst){
  
  if(!grepl(sprintf("^0 %s SUBM$", reg_xref(FALSE)), records_lst[[2]][1]))
    stop("The record immediately after the header record must be the Submitter record")
  
  if(records_lst[[length(records_lst)]][1] != "0 TRLR")
    stop("The file does not end with a TRLR record")
  
  records_lst <- records_lst[-length(records_lst)]
  
  # parse subm and header
  x <- create_gedcom(records_lst)
  records_lst <- records_lst[-(1:2)]
  
  #x <- do_it(x, records_lst)
  # for(i in seq_along(records_lst)){
  #   x <- parse_record(x, records_lst[[i]])
  # }
  x <- Reduce(parse_record, records_lst, init = x)
  x
}

do_it <- function(x, records_lst){
  R7::valid_eventually(x, function(object) {
    Reduce(parse_record, records_lst, init = object)
  })
}

create_gedcom <- function(records_lst){
  
  subm_lines <- records_lst[[2]]

  subm_nts <- extract_ged_values(subm_lines, "NOTE")
  
  subm <- class_record_subm(
    xref = extract_ged_values(subm_lines, return_xref = TRUE),
    name = extract_ged_values(subm_lines, "NAME"),
    address = extract_address(subm_lines),
    media_links = extract_ged_values(subm_lines, "OBJE"),
    auto_id = extract_ged_values(subm_lines, "RIN"),
    note_links = subm_nts[grepl(reg_xref(TRUE), subm_nts)],
    notes = subm_nts[!grepl(reg_xref(TRUE), subm_nts)],
    last_updated = extract_change_date(subm_lines)
  )
  
  hd_lines <- records_lst[[1]]
  
  class_gedcomR7(
    system_id = extract_ged_values(hd_lines, c("HEAD","SOUR")),
    product_name = extract_ged_values(hd_lines, c("HEAD","SOUR","NAME")),
    product_version = extract_ged_values(hd_lines, c("HEAD","SOUR","VERS")),
    business_name = extract_ged_values(hd_lines, c("HEAD","SOUR","CORP")),
    business_address = extract_address(hd_lines, c("HEAD","SOUR","CORP")),
    source_data_name = extract_ged_values(hd_lines, c("HEAD","SOUR","DATA")),
    source_data_pubdate = extract_ged_values(hd_lines, c("HEAD","SOUR","DATA","DATE")),
    source_data_copyright = extract_ged_values(hd_lines, c("HEAD","SOUR","DATA","COPR")),
    receiving_system = extract_ged_values(hd_lines, c("HEAD","DEST")),
    creation_date = extract_ged_values(hd_lines, c("HEAD","DATE")),
    creation_time = extract_ged_values(hd_lines, c("HEAD","DATE","TIME")),
    language = extract_ged_values(hd_lines, c("HEAD","LANG")),
    xref_subm = extract_ged_values(hd_lines, c("HEAD","SUBM")),
    file_name = extract_ged_values(hd_lines, c("HEAD","FILE")),
    gedcom_copyright = extract_ged_values(hd_lines, c("HEAD","COPR")),
    content_description = extract_ged_values(hd_lines, c("HEAD","NOTE")),
    subm = subm
  )
  
}

parse_record <- function(x, rec_lines){
  
  rec_type <- sub(sprintf("^0 %s ([A-Z]{3,5})( .*)?$", reg_xref(FALSE)), "\\1", rec_lines[1])
  rec_xref <- extract_ged_values(rec_lines, return_xref = TRUE)
  if(rec_type == "NOTE") #special case
    note_text <- sub(sprintf("^0 %s NOTE (.+)$", reg_xref(FALSE)), "\\1", rec_lines[1])
  
  nts <- extract_ged_values(rec_lines, "NOTE")
  refns <- extract_refns(rec_lines)
  chan <- extract_change_date(rec_lines) 
  media <- extract_ged_values(rec_lines, "OBJE")
  cits <- extract_citations(rec_lines)
  auto_id <- extract_ged_values(rec_lines, "RIN")
  
  if(rec_type == "INDI"){
    
    x@indi[[rec_xref]] <- class_record_indi(
      xref = rec_xref,
      sex = extract_ged_values(rec_lines, "SEX"),
      user_reference_numbers = refns,
      notes = nts[!grepl(reg_xref(TRUE), nts)],
      personal_names = extract_personal_names(rec_lines),
      facts = extract_facts_indi(rec_lines),
      family_links = extract_family_links(rec_lines),
      associations = extract_associations(rec_lines),
      auto_id = auto_id,
      last_updated = chan,
      note_links = nts[grepl(reg_xref(TRUE), nts)],
      citations = cits,
      media_links = media
    )

  } else if(rec_type == "FAM"){
    
    x@famg[[rec_xref]] <- class_record_famg(
      xref = rec_xref,
      husb_xref = extract_ged_values(rec_lines, "HUSB"),
      wife_xref = extract_ged_values(rec_lines, "WIFE"),
      chil_xref = extract_ged_values(rec_lines, "CHIL"),
      user_reference_numbers = refns, 
      notes = nts[!grepl(reg_xref(TRUE), nts)], 
      facts = extract_facts_famg(rec_lines),
      num_children = as.integer(extract_ged_values(rec_lines, "NCHI")),
      auto_id = auto_id,
      last_updated = chan,
      note_links = nts[grepl(reg_xref(TRUE), nts)],
      citations = cits,
      media_links = media
    )
    
  } else if(rec_type == "SOUR"){
    
    data_nts <- extract_ged_values(rec_lines, c("DATA","NOTE"))
    
    x@sour[[rec_xref]] <- class_record_sour(
      xref = rec_xref,
      title = extract_ged_values(rec_lines, "TITL"),
      user_reference_numbers = refns, 
      notes = nts[!grepl(reg_xref(TRUE), nts)],
      events_recorded = extract_ged_values(rec_lines, c("DATA","EVEN")),
      date_period = extract_ged_values(rec_lines, c("DATA","EVEN","DATE")),
      jurisdiction_place = extract_ged_values(rec_lines, c("DATA","EVEN","PLAC")),
      responsible_agency = extract_ged_values(rec_lines, c("DATA","AGNC")),
      data_note_links = data_nts[grepl(reg_xref(TRUE), data_nts)],
      data_notes = data_nts[!grepl(reg_xref(TRUE), data_nts)],
      originator = extract_ged_values(rec_lines, "AUTH"),
      short_title = extract_ged_values(rec_lines, "ABBR"),
      publication_facts = extract_ged_values(rec_lines, "PUBL"),
      source_text = extract_ged_values(rec_lines, "TEXT"),
      repo_citations = extract_repo_citations(rec_lines),
      auto_id = auto_id,
      last_updated = chan,
      note_links = nts[grepl(reg_xref(TRUE), nts)],
      media_links = media
    )
    
  } else if(rec_type == "REPO"){
    
    x@repo[[rec_xref]] <- class_record_repo(
      xref = rec_xref,
      name = extract_ged_values(rec_lines, "NAME"), 
      user_reference_numbers = refns, 
      notes = nts[!grepl(reg_xref(TRUE), nts)],
      address = extract_address(rec_lines),
      note_links = nts[grepl(reg_xref(TRUE), nts)],
      auto_id = auto_id,
      last_updated = chan
    )
    
  } else if(rec_type == "OBJE"){
    
    x@media[[rec_xref]] <- class_record_media(
      xref = rec_xref,
      file_ref = extract_ged_values(rec_lines, "FILE"),
      format = extract_ged_values(rec_lines, c("FILE","FORM")),
      user_reference_numbers = refns,
      notes = nts[!grepl(reg_xref(TRUE), nts)],
      media_type = extract_ged_values(rec_lines, c("FILE","FORM","TYPE")),
      title = extract_ged_values(rec_lines, c("FILE","TITL")),
      auto_id = auto_id,
      note_links = nts[grepl(reg_xref(TRUE), nts)],
      citations = cits,
      last_updated = chan
    )
    
  } else if(rec_type == "NOTE"){
    
    x@note[[rec_xref]] <- class_record_note(
      xref = rec_xref,
      text = note_text, 
      user_reference_numbers = refns, 
      auto_id = auto_id,
      citations = cits,
      last_updated = chan
    )
  }
  
  x
  
}


extract_ged_values <- function(lines, 
                               tag,
                               return_xref = FALSE,
                               return_list = FALSE){
  
  if(return_xref)
    return(sub(sprintf("^0 (%s) .+$", reg_xref(FALSE)), "\\1", lines[1]))
  
  base_level <- as.integer(substr(lines[1], 1, 1)) - 1
  
  # Ignore parent if lines describes a whole record
  if(grepl(sprintf("^0 (%s) .+$", reg_xref(FALSE)), lines[1])){
    lines <- lines[-1]
    base_level <- base_level + 1
  }
  
  if(length(tag) > length(lines)) return(character())
  
  for(level in seq_along(tag)){
    
    lines_lst <- split(lines, cumsum(substr(lines, 1, 1) == as.character(base_level + level)))
    
    lines_lst <- Filter(\(x) grepl(sprintf("^%s (%s)( .*)?$", base_level + level, tag[level]), x[1]), 
                        lines_lst)
    
    if(level == length(tag)){ # final tag
      if(return_list){
        return(unname(lines_lst))
      } else {
      # strip out subordinates
      lines_lst <- lapply(lines_lst, \(x) x[1])
      }
    } else { # remove parent tag ready for splitting again
      lines_lst <- lapply(lines_lst, \(x) x[-1])
    }
    
    if(length(lines_lst) == 0) return(character())
    
    lines <- unlist(lines_lst)
  }
  
  lines <- unname(lines)
  # Catch cases where no line value is given
  lines <- lines[lines != paste(base_level + length(tag), tag[length(tag)])]
  sub(sprintf("^%s (%s) (.*)$", base_level + length(tag), tag[length(tag)]), "\\2", lines)
}


extract_refns <- function(rec_lines){
  refn_lst <- extract_ged_values(rec_lines, "REFN", return_list = TRUE)
  if(length(refn_lst) == 0) return(character())
  
  refns <- sapply(refn_lst, \(x) sub("^1 REFN (.*)$", "\\1", x[1]))
  types <- sapply(refn_lst, \(x) {
    if(length(x) == 1) return("")
    sub("^2 TYPE (.*)$", "\\1", x[2])
  })
  names(refns) <- types
  refns
}

extract_change_date <- function(rec_lines){
  change_date <- extract_ged_values(rec_lines, c("CHAN","DATE"))
  if(length(change_date) == 0) return(NULL)
  
  nts <- extract_ged_values(rec_lines, c("CHAN","NOTE"))
  class_change_date(
    date = change_date,
    time = extract_ged_values(rec_lines, c("CHAN","DATE","TIME")),
    notes = nts[!grepl(reg_xref(TRUE), nts)],
    note_links = nts[grepl(reg_xref(TRUE), nts)]
  )
}

extract_citations <- function(rec_lines, location = NULL){
  
  sour_lst <- extract_ged_values(rec_lines, c(location, "SOUR"), return_list = TRUE)
  if(length(sour_lst) == 0) return(list())
  
  lapply(sour_lst, \(x){
    nts <- extract_ged_values(x, c("SOUR","NOTE"))
    
    class_citation(
      xref = extract_ged_values(x, "SOUR"),
      where = extract_ged_values(x, c("SOUR","PAGE")),
      event_type = extract_ged_values(x, c("SOUR","EVEN")),
      event_role = extract_ged_values(x, c("SOUR","EVEN","ROLE")),
      recording_date = extract_ged_values(x, c("SOUR","DATA","DATE")),
      source_text = extract_ged_values(x, c("SOUR","DATA","TEXT")),
      media_links = extract_ged_values(x, c("SOUR","OBJE")),
      note_links = nts[grepl(reg_xref(TRUE), nts)],
      notes = nts[!grepl(reg_xref(TRUE), nts)],
      certainty = extract_ged_values(x, c("SOUR","QUAY"))
    )
  })
  
}

extract_personal_names <- function(rec_lines){
  
  name_lst <- extract_ged_values(rec_lines, "NAME", return_list = TRUE)
  if(length(name_lst) == 0) return(list())
  
  lapply(name_lst, \(x){
    nm <- extract_name_info(x, "NAME")
    phon_lst <- extract_ged_values(x, c("NAME","FONE"), return_list = TRUE)
    rom_lst <- extract_ged_values(x, c("NAME","ROMN"), return_list = TRUE)
    
    class_personal_name(
      name = nm,
      phon_names = lapply(phon_lst, extract_name_info, "FONE"),
      rom_names = lapply(rom_lst, extract_name_info, "ROMN")
    )
  })
}

extract_name_info <- function(lines, location){
  
  nts <- extract_ged_values(lines, c(location, "NOTE"))
  
  surn <- extract_ged_values(lines, c(location, "SURN"))
  full <- extract_ged_values(lines, location)
  if(length(surn) == 0){
    surn <- sub("^.*/(.+)/.*$", "\\1", full)
  }
  
  class_name_info(
    full = full,
    type = extract_ged_values(lines, c(location, "TYPE")),
    prefix = extract_ged_values(lines, c(location, "NPFX")),
    given = extract_ged_values(lines, c(location, "GIVN")),
    nickname = extract_ged_values(lines, c(location, "NICK")),
    surname_prefix = extract_ged_values(lines, c(location, "SPFX")),
    surname = surn,
    suffix = extract_ged_values(lines, c(location, "NSFX")),
    note_links = nts[grepl(reg_xref(TRUE), nts)],
    notes = nts[!grepl(reg_xref(TRUE), nts)],
    citations = extract_citations(lines[-1])
  )
  
}


extract_associations <- function(rec_lines){
  asso_lst <- extract_ged_values(rec_lines, "ASSO", return_list = TRUE)
  if(length(asso_lst) == 0) return(list())
  
  lapply(asso_lst, \(x){
    nts <- extract_ged_values(x, c("ASSO","NOTE"))
    
    class_association(
      xref = extract_ged_values(x, "ASSO"),
      relation_is = extract_ged_values(x, c("ASSO","RELA")),
      citations = extract_citations(x, "ASSO"),
      note_links = nts[grepl(reg_xref(TRUE), nts)],
      notes = nts[!grepl(reg_xref(TRUE), nts)]
    )
  })
}

extract_repo_citations <- function(rec_lines){
  repo_lst <- extract_ged_values(rec_lines, "REPO", return_list = TRUE) 
  if(length(repo_lst) == 0) return(list())
  
  lapply(repo_lst, \(x){
    class_repository_citation(
      xref = extract_ged_values(x, "REPO"),
      source_call_number = extract_ged_values(x, c("REPO","CALN"))
    )
  })
}

extract_place <- function(lines, location = NULL){
  
  place_name <- extract_ged_values(lines, c(location, "PLAC"))
  if(length(place_name) == 0) return(NULL)
  
  nts <- extract_ged_values(lines, c(location, "PLAC", "NOTE"))
  latlong <- paste(
    extract_ged_values(lines, c(location, "MAP", "LATI")),
    extract_ged_values(lines, c(location, "MAP", "LONG"))
  )
  
  class_place(
    name = place_name,
    phon_names = character(),#TODO
    rom_names = character(),#TODO
    lat_long = latlong,
    note_links = nts[grepl(reg_xref(TRUE), nts)],
    notes = nts[!grepl(reg_xref(TRUE), nts)]
  )
  
}

extract_facts_indi <- function(rec_lines){
  fact_lst <- extract_ged_values(rec_lines, return_list = TRUE,
                                 tag = paste(c(val_attribute_types(),
                                               val_individual_event_types()),
                                             collapse = "|"))
  if(length(fact_lst) == 0) return(list())
  
  lapply(fact_lst, \(x){
    tag <- sub("^1 ([A-Z]{3,4})( .+)?$", "\\1", x[1])
    
    nts <- extract_ged_values(x, c(tag, "NOTE"))
    
    class_fact_indi(
      fact = tag,
      description = extract_ged_values(x, tag),
      age = extract_ged_values(x, c(tag, "AGE")),
      famg_xref = extract_ged_values(x, c(tag, "FAMC")),
      adopting_parent = extract_ged_values(x, c(tag, "FAMC","ADOP")),
      type = extract_ged_values(x, c(tag, "TYPE")),
      date = extract_ged_values(x, c(tag, "DATE")),
      place = extract_place(x, tag),
      address = extract_address(x, tag),
      agency = extract_ged_values(x, c(tag, "AGNC")),
      relig_affil = extract_ged_values(x, c(tag, "RELI")),
      cause = extract_ged_values(x, c(tag, "CAUS")),
      note_links = nts[grepl(reg_xref(TRUE), nts)],
      notes = nts[!grepl(reg_xref(TRUE), nts)],
      citations = extract_citations(x, tag),
      media_links = extract_ged_values(x, c(tag, "OBJE"))
    )
  })
}

extract_facts_famg <- function(rec_lines){
  fact_lst <- extract_ged_values(rec_lines, return_list = TRUE,
                                 tag = paste(val_family_event_types(),
                                             collapse = "|"))
  if(length(fact_lst) == 0) return(list())
  
  lapply(fact_lst, \(x){
    tag <- sub("^1 ([A-Z]{3,4})( .+)?$", "\\1", x[1])

    nts <- extract_ged_values(x, c(tag, "NOTE"))
    
    class_fact_famg(
      fact = tag,
      description = extract_ged_values(x, tag),
      husband_age = extract_ged_values(x, c(tag, "HUSB","AGE")),
      wife_age = extract_ged_values(x, c(tag, "WIFE","AGE")),
      type = extract_ged_values(x, c(tag, "TYPE")),
      date = extract_ged_values(x, c(tag, "DATE")),
      place = extract_place(x, tag),
      address = extract_address(x, tag),
      agency = extract_ged_values(x, c(tag, "AGNC")),
      relig_affil = extract_ged_values(x, c(tag, "RELI")),
      cause = extract_ged_values(x, c(tag, "CAUS")),
      note_links = nts[grepl(reg_xref(TRUE), nts)],
      notes = nts[!grepl(reg_xref(TRUE), nts)],
      citations = extract_citations(x, tag),
      media_links = extract_ged_values(x, c(tag, "OBJE"))
    )
  })
}

extract_address <- function(lines, location = NULL){
  
  class_address(
    local_address_lines = c(extract_ged_values(lines, c(location, "ADDR","ADR1")),
                            extract_ged_values(lines, c(location, "ADDR","ADR2")),
                            extract_ged_values(lines, c(location, "ADDR","ADR3"))),
    city = extract_ged_values(lines, c(location, "ADDR","CITY")),
    state = extract_ged_values(lines, c(location, "ADDR","STAE")),
    postal_code = extract_ged_values(lines, c(location, "ADDR","POST")),
    country = extract_ged_values(lines, c(location, "ADDR","CTRY")),
    emails = extract_ged_values(lines, c(location, "EMAIL")),
    faxes = extract_ged_values(lines, c(location, "FAX")),
    phone_numbers = extract_ged_values(lines, c(location, "PHON")),
    web_pages = extract_ged_values(lines, c(location, "WWW|URL"))
  )
}



extract_family_links <- function(rec_lines){
  link_lst <- extract_ged_values(rec_lines, "FAMS|FAMC", return_list = TRUE) 
  if(length(link_lst) == 0) return(list())
  
  lapply(link_lst, \(x){
    nts <- extract_ged_values(x, c("FAMS|FAMC", "NOTE"))
    xref <- extract_ged_values(x, "FAMC|FAMS")
    
    if(grepl("FAMC", x[1])){
      class_child_to_family_link(
        xref = xref,
        pedigree = extract_ged_values(x, c("FAMC", "PEDI")),
        note_links = nts[grepl(reg_xref(TRUE), nts)],
        notes = nts[!grepl(reg_xref(TRUE), nts)]
      )
    } else {
      class_spouse_to_family_link(
        xref = xref,
        note_links = nts[grepl(reg_xref(TRUE), nts)],
        notes = nts[!grepl(reg_xref(TRUE), nts)]
      )
    }
    
  })
}


#' Capitalise tags and certain keywords
#' 
#' This function capitalises all tags and certain values such as SEX values and DATE values.
#' 
#' @details The function also ensures certain values are lowercase such as PEDI
#' and ADOP values, and removes explicit GREGORIAN date escape sequences (as they are implied).
#'
#' @param gedcom A tidyged object.
#'
#' @return A tidyged object with appropriately capitalised tags and keywords.
capitalise_tags_and_keywords <- function(gedcom){
  
  gedcom |> 
    dplyr::mutate(tag = toupper(tag)) |> 
    dplyr::mutate(value = dplyr::if_else(tag == "SEX", toupper(value), value),
                  value = dplyr::if_else(tag == "PEDI", tolower(value), value),
                  value = dplyr::if_else(tag == "ADOP", toupper(value), value),
                  value = dplyr::if_else(level == 2 & tag == "EVEN", toupper(value), value),
                  value = dplyr::if_else(tag == "LATI", toupper(value), value),
                  value = dplyr::if_else(tag == "LONG", toupper(value), value),
                  value = dplyr::if_else(tag == "ROLE" & 
                                           !stringr::str_detect(value, tidyged.internals::reg_custom_value()), 
                                         toupper(value), value),
                  value = dplyr::if_else(tag == "DATE" &
                                           !stringr::str_detect(value, tidyged.internals::reg_custom_value()), 
                                         toupper(value), value),
                  value = dplyr::if_else(tag == "DATE" &
                                           !stringr::str_detect(value, tidyged.internals::reg_custom_value()),
                                         stringr::str_remove(value, "@#DGREGORIAN@ "), value)) |> 
    dplyr::mutate(tag = dplyr::if_else(tag == "URL", "WWW", tag))
  
  
}

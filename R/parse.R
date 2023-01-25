
pull_record <- function(rec_lines){
  
  rec_type <- extract_ged_tag(rec_lines[1])
  rec_xref <- extract_ged_xref(rec_lines[1])
  
  if(rec_type == "NOTE") #special case
    note_text <- extract_ged_value(rec_lines[1])
  
  nts <- find_ged_values(rec_lines, "NOTE")
  refns <- extract_refns(rec_lines)
  chan <- extract_change_date(rec_lines) 
  media <- find_ged_values(rec_lines, "OBJE")
  cits <- extract_citations(rec_lines)
  auto_id <- find_ged_values(rec_lines, "RIN")
  
  if(rec_type == "INDI"){
    
    rec <- class_record_indi(
      xref = rec_xref,
      sex = toupper(find_ged_values(rec_lines, "SEX")),
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
    
    rec <- class_record_famg(
      xref = rec_xref,
      husb_xref = find_ged_values(rec_lines, "HUSB"),
      wife_xref = find_ged_values(rec_lines, "WIFE"),
      chil_xref = find_ged_values(rec_lines, "CHIL"),
      user_reference_numbers = refns, 
      notes = nts[!grepl(reg_xref(TRUE), nts)], 
      facts = extract_facts_famg(rec_lines),
      num_children = as.integer(find_ged_values(rec_lines, "NCHI")),
      auto_id = auto_id,
      last_updated = chan,
      note_links = nts[grepl(reg_xref(TRUE), nts)],
      citations = cits,
      media_links = media
    )
    
  } else if(rec_type == "SOUR"){
    
    data_nts <- find_ged_values(rec_lines, c("DATA","NOTE"))
    
    rec <- class_record_sour(
      xref = rec_xref,
      full_title = find_ged_values(rec_lines, "TITL"),
      user_reference_numbers = refns, 
      notes = nts[!grepl(reg_xref(TRUE), nts)],
      events_recorded = toupper(find_ged_values(rec_lines, c("DATA","EVEN"))),
      date_period = find_ged_values(rec_lines, c("DATA","EVEN","DATE")),
      jurisdiction_place = find_ged_values(rec_lines, c("DATA","EVEN","PLAC")),
      responsible_agency = find_ged_values(rec_lines, c("DATA","AGNC")),
      data_note_links = data_nts[grepl(reg_xref(TRUE), data_nts)],
      data_notes = data_nts[!grepl(reg_xref(TRUE), data_nts)],
      originator = find_ged_values(rec_lines, "AUTH"),
      short_title = find_ged_values(rec_lines, "ABBR"),
      publication_facts = find_ged_values(rec_lines, "PUBL"),
      source_text = find_ged_values(rec_lines, "TEXT"),
      repo_citations = extract_repo_citations(rec_lines),
      auto_id = auto_id,
      last_updated = chan,
      note_links = nts[grepl(reg_xref(TRUE), nts)],
      media_links = media
    )
    
  } else if(rec_type == "REPO"){
    
    rec <- class_record_repo(
      xref = rec_xref,
      name = find_ged_values(rec_lines, "NAME"), 
      user_reference_numbers = refns, 
      notes = nts[!grepl(reg_xref(TRUE), nts)],
      address = extract_address(rec_lines),
      note_links = nts[grepl(reg_xref(TRUE), nts)],
      auto_id = auto_id,
      last_updated = chan
    )
    
  } else if(rec_type == "OBJE"){
    
    rec <- class_record_media(
      xref = rec_xref,
      file_ref = find_ged_values(rec_lines, "FILE"),
      format = find_ged_values(rec_lines, c("FILE","FORM")),
      user_reference_numbers = refns,
      notes = nts[!grepl(reg_xref(TRUE), nts)],
      media_type = find_ged_values(rec_lines, c("FILE","FORM","TYPE")),
      title = find_ged_values(rec_lines, c("FILE","TITL")),
      auto_id = auto_id,
      note_links = nts[grepl(reg_xref(TRUE), nts)],
      citations = cits,
      last_updated = chan
    )
    
  } else if(rec_type == "NOTE"){
    
    rec <- class_record_note(
      xref = rec_xref,
      text = note_text, 
      user_reference_numbers = refns, 
      auto_id = auto_id,
      citations = cits,
      last_updated = chan
    )
  }
  
  rec
  
}




extract_refns <- function(rec_lines){
  refn_lst <- find_ged_values(rec_lines, "REFN", return_list = TRUE)
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
  change_date <- find_ged_values(rec_lines, c("CHAN","DATE"))
  if(length(change_date) == 0) return(NULL)
  
  nts <- find_ged_values(rec_lines, c("CHAN","NOTE"))
  class_change_date(
    date = change_date,
    time = find_ged_values(rec_lines, c("CHAN","DATE","TIME")),
    notes = nts[!grepl(reg_xref(TRUE), nts)],
    note_links = nts[grepl(reg_xref(TRUE), nts)]
  )
}

extract_citations <- function(rec_lines, location = NULL){
  
  sour_lst <- find_ged_values(rec_lines, c(location, "SOUR"), return_list = TRUE)
  if(length(sour_lst) == 0) return(list())
  
  lapply(sour_lst, \(x){
    nts <- find_ged_values(x, c("SOUR","NOTE"))
    rec_date <- find_ged_values(x, c("SOUR","DATA","DATE"))
    if(length(rec_date) == 1 && !grepl(reg_custom_value(), rec_date)){
      rec_date <- toupper(rec_date)
      rec_date <- sub("@#DGREGORIAN@ ", "", rec_date)
    }
    role <- find_ged_values(x, c("SOUR","EVEN","ROLE"))
    if(length(role) == 1 && !grepl(reg_custom_value(), role)){
      role <- toupper(role)
    }
    
    class_citation(
      xref = find_ged_values(x, "SOUR"),
      where = find_ged_values(x, c("SOUR","PAGE")),
      event_type = find_ged_values(x, c("SOUR","EVEN")),
      event_role = role,
      recording_date = rec_date,
      source_text = find_ged_values(x, c("SOUR","DATA","TEXT")),
      media_links = find_ged_values(x, c("SOUR","OBJE")),
      note_links = nts[grepl(reg_xref(TRUE), nts)],
      notes = nts[!grepl(reg_xref(TRUE), nts)],
      certainty = find_ged_values(x, c("SOUR","QUAY"))
    )
  })
  
}

extract_personal_names <- function(rec_lines){
  
  name_lst <- find_ged_values(rec_lines, "NAME", return_list = TRUE)
  if(length(name_lst) == 0) return(list())
  
  lapply(name_lst, \(x){
    nm <- extract_name_info(x, "NAME")
    phon_lst <- find_ged_values(x, c("NAME","FONE"), return_list = TRUE)
    rom_lst <- find_ged_values(x, c("NAME","ROMN"), return_list = TRUE)
    
    class_personal_name(
      name = nm,
      phon_names = lapply(phon_lst, extract_name_info, "FONE"),
      rom_names = lapply(rom_lst, extract_name_info, "ROMN")
    )
  })
}

extract_name_info <- function(lines, location){
  
  nts <- find_ged_values(lines, c(location, "NOTE"))
  
  surn <- find_ged_values(lines, c(location, "SURN"))
  full <- find_ged_values(lines, location)
  if(length(surn) == 0){
    surn <- sub("^.*/(.+)/.*$", "\\1", full)
  }
  
  class_name_info(
    full = full,
    type = find_ged_values(lines, c(location, "TYPE")),
    prefix = find_ged_values(lines, c(location, "NPFX")),
    given = find_ged_values(lines, c(location, "GIVN")),
    nickname = find_ged_values(lines, c(location, "NICK")),
    surname_prefix = find_ged_values(lines, c(location, "SPFX")),
    surname = surn,
    suffix = find_ged_values(lines, c(location, "NSFX")),
    note_links = nts[grepl(reg_xref(TRUE), nts)],
    notes = nts[!grepl(reg_xref(TRUE), nts)],
    citations = extract_citations(lines[-1])
  )
  
}


extract_associations <- function(rec_lines){
  asso_lst <- find_ged_values(rec_lines, "ASSO", return_list = TRUE)
  if(length(asso_lst) == 0) return(list())
  
  lapply(asso_lst, \(x){
    nts <- find_ged_values(x, c("ASSO","NOTE"))
    
    class_association(
      xref = find_ged_values(x, "ASSO"),
      relation_is = find_ged_values(x, c("ASSO","RELA")),
      citations = extract_citations(x, "ASSO"),
      note_links = nts[grepl(reg_xref(TRUE), nts)],
      notes = nts[!grepl(reg_xref(TRUE), nts)]
    )
  })
}

extract_repo_citations <- function(rec_lines){
  repo_lst <- find_ged_values(rec_lines, "REPO", return_list = TRUE) 
  if(length(repo_lst) == 0) return(list())
  
  lapply(repo_lst, \(x){
    class_repository_citation(
      xref = find_ged_values(x, "REPO"),
      source_call_number = find_ged_values(x, c("REPO","CALN"))
    )
  })
}

extract_place <- function(lines, location = NULL){
  
  place_name <- find_ged_values(lines, c(location, "PLAC"))
  if(length(place_name) == 0) return(NULL)
  
  nts <- find_ged_values(lines, c(location, "PLAC", "NOTE"))
  latlong <- paste(
    find_ged_values(lines, c(location, "MAP", "LATI")),
    find_ged_values(lines, c(location, "MAP", "LONG"))
  )
  
  class_place(
    name = place_name,
    phon_names = character(),#TODO
    rom_names = character(),#TODO
    lat_long = toupper(latlong),
    note_links = nts[grepl(reg_xref(TRUE), nts)],
    notes = nts[!grepl(reg_xref(TRUE), nts)]
  )
  
}

extract_facts_indi <- function(rec_lines){
  fact_lst <- find_ged_values(rec_lines, return_list = TRUE,
                                 tag = paste(c(val_attribute_types(),
                                               val_individual_event_types()),
                                             collapse = "|"))
  if(length(fact_lst) == 0) return(list())
  
  lapply(fact_lst, \(x){
    tag <- extract_ged_tag(x[1])
    
    nts <- find_ged_values(x, c(tag, "NOTE"))
    fact_date <- find_ged_values(x, c(tag, "DATE"))
    if(length(fact_date) == 1 && !grepl(reg_custom_value(), fact_date)){
      fact_date <- toupper(fact_date)
      fact_date <- sub("@#DGREGORIAN@ ", "", fact_date)
    }
    
    class_fact_indi(
      fact = tag,
      description = find_ged_values(x, tag),
      age = find_ged_values(x, c(tag, "AGE")),
      famg_xref = find_ged_values(x, c(tag, "FAMC")),
      adopting_parent = toupper(find_ged_values(x, c(tag, "FAMC","ADOP"))),
      type = find_ged_values(x, c(tag, "TYPE")),
      date = fact_date,
      place = extract_place(x, tag),
      address = extract_address(x, tag),
      agency = find_ged_values(x, c(tag, "AGNC")),
      relig_affil = find_ged_values(x, c(tag, "RELI")),
      cause = find_ged_values(x, c(tag, "CAUS")),
      note_links = nts[grepl(reg_xref(TRUE), nts)],
      notes = nts[!grepl(reg_xref(TRUE), nts)],
      citations = extract_citations(x, tag),
      media_links = find_ged_values(x, c(tag, "OBJE"))
    )
  })
}

extract_facts_famg <- function(rec_lines){
  fact_lst <- find_ged_values(rec_lines, return_list = TRUE,
                                 tag = paste(val_family_event_types(),
                                             collapse = "|"))
  if(length(fact_lst) == 0) return(list())
  
  lapply(fact_lst, \(x){
    tag <- extract_ged_tag(x[1])
    
    nts <- find_ged_values(x, c(tag, "NOTE"))
    fact_date <- find_ged_values(x, c(tag, "DATE"))
    if(length(fact_date) == 1 && !grepl(reg_custom_value(), fact_date)){
      fact_date <- toupper(fact_date)
      fact_date <- sub("@#DGREGORIAN@ ", "", fact_date)
    }
    
    class_fact_famg(
      fact = tag,
      description = find_ged_values(x, tag),
      husband_age = find_ged_values(x, c(tag, "HUSB","AGE")),
      wife_age = find_ged_values(x, c(tag, "WIFE","AGE")),
      type = find_ged_values(x, c(tag, "TYPE")),
      date = fact_date,
      place = extract_place(x, tag),
      address = extract_address(x, tag),
      agency = find_ged_values(x, c(tag, "AGNC")),
      relig_affil = find_ged_values(x, c(tag, "RELI")),
      cause = find_ged_values(x, c(tag, "CAUS")),
      note_links = nts[grepl(reg_xref(TRUE), nts)],
      notes = nts[!grepl(reg_xref(TRUE), nts)],
      citations = extract_citations(x, tag),
      media_links = find_ged_values(x, c(tag, "OBJE"))
    )
  })
}

extract_address <- function(lines, location = NULL){
  
  class_address(
    local_address_lines = c(find_ged_values(lines, c(location, "ADDR","ADR1")),
                            find_ged_values(lines, c(location, "ADDR","ADR2")),
                            find_ged_values(lines, c(location, "ADDR","ADR3"))),
    city = find_ged_values(lines, c(location, "ADDR","CITY")),
    state = find_ged_values(lines, c(location, "ADDR","STAE")),
    postal_code = find_ged_values(lines, c(location, "ADDR","POST")),
    country = find_ged_values(lines, c(location, "ADDR","CTRY")),
    emails = find_ged_values(lines, c(location, "EMAIL")),
    faxes = find_ged_values(lines, c(location, "FAX")),
    phone_numbers = find_ged_values(lines, c(location, "PHON")),
    web_pages = find_ged_values(lines, c(location, "WWW|URL"))
  )
}



extract_family_links <- function(rec_lines){
  link_lst <- find_ged_values(rec_lines, "FAMS|FAMC", return_list = TRUE) 
  if(length(link_lst) == 0) return(list())
  
  lapply(link_lst, \(x){
    nts <- find_ged_values(x, c("FAMS|FAMC", "NOTE"))
    xref <- find_ged_values(x, "FAMC|FAMS")
    
    if(grepl("FAMC", x[1], fixed = TRUE)){
      class_child_to_family_link(
        xref = xref,
        pedigree = tolower(find_ged_values(x, c("FAMC", "PEDI"))),
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


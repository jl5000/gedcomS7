
#' Pull a record from a GEDCOM object for editing
#' 
#' @details The record is not removed from the gedcom object, rather a copy is taken.
#'
#' @param x A gedcom object.
#' @param xref The xref of the record to pull.
#'
#' @return An S7 object representing the record.
#' @export
pull_record <- function(x, xref){
  
  rec_lines <- c(x@indi, x@famg, x@sour,
                 x@repo, x@media, x@note)[[xref]]
  
  rec_type <- extract_ged_tag(rec_lines[1])
  if(!rec_type %in% c("INDI","FAM","SOUR","REPO","NOTE","OBJE"))
    stop("Record type not recognised: ", rec_type)
  
  rec_xref <- extract_ged_xref(rec_lines[1])
  
  nts <- find_ged_values(rec_lines, "NOTE")
  refns <- extract_vals_and_types(rec_lines, "REFN")
  chan <- extract_change_date(rec_lines) 
  media <- find_ged_values(rec_lines, "OBJE")
  cits <- extract_citations(rec_lines)
  auto_id <- find_ged_values(rec_lines, "RIN")
  
  if(rec_type == "INDI"){
    
    class_record_indi(
      xref = rec_xref,
      sex = toupper(find_ged_values(rec_lines, "SEX")),
      user_reference_numbers = refns,
      notes = nts[!grepl(reg_xref(TRUE), nts)],
      personal_names = extract_personal_names(rec_lines),
      facts = extract_facts_indi(rec_lines),
      family_links = extract_family_links(rec_lines),
      associations = extract_associations(rec_lines),
      auto_id = auto_id,
      updated = chan,
      note_links = nts[grepl(reg_xref(TRUE), nts)],
      citations = cits,
      media_links = media
    )
    
  } else if(rec_type == "FAM"){
    
    chil_xref <- find_ged_values(rec_lines, "CHIL")
    biol_xref <- adop_xref <- fost_xref <- character()
    for(chil in chil_xref){
      chil_lines <- x@indi[[chil]]
      links <- extract_family_links(chil_lines)
      
      for(lnk in links){
        if(lnk@xref == rec_xref){
          if(is_adop_child_link(lnk)){
            adop_xref <- c(adop_xref, chil)
          } else if(is_fost_child_link(lnk)){
            fost_xref <- c(fost_xref, chil)
          } else if(is_birth_child_link(lnk)){
            biol_xref <- c(biol_xref, chil)
          }
        }
       
      }
    }
    
    class_record_fam(
      xref = rec_xref,
      husb_xref = find_ged_values(rec_lines, "HUSB"),
      wife_xref = find_ged_values(rec_lines, "WIFE"),
      chil_biol_xref = biol_xref,
      chil_adop_xref = adop_xref,
      chil_fost_xref = fost_xref,
      user_reference_numbers = refns, 
      notes = nts[!grepl(reg_xref(TRUE), nts)], 
      facts = extract_facts_famg(rec_lines),
      num_children = as.integer(find_ged_values(rec_lines, "NCHI")),
      auto_id = auto_id,
      updated = chan,
      note_links = nts[grepl(reg_xref(TRUE), nts)],
      citations = cits,
      media_links = media
    )
    
  } else if(rec_type == "SOUR"){
    
    data_nts <- find_ged_values(rec_lines, c("DATA","NOTE"))
    
    class_record_sour(
      xref = rec_xref,
      full_title = find_ged_values(rec_lines, "TITL"),
      user_reference_numbers = refns, 
      notes = nts[!grepl(reg_xref(TRUE), nts)],
      facts_recorded = extract_events_recorded(rec_lines),
      responsible_agency = find_ged_values(rec_lines, c("DATA","AGNC")),
      data_note_links = data_nts[grepl(reg_xref(TRUE), data_nts)],
      data_notes = data_nts[!grepl(reg_xref(TRUE), data_nts)],
      originator = find_ged_values(rec_lines, "AUTH"),
      short_title = find_ged_values(rec_lines, "ABBR"),
      publication_facts = find_ged_values(rec_lines, "PUBL"),
      source_text = find_ged_values(rec_lines, "TEXT"),
      repo_citations = extract_repo_citations(rec_lines),
      auto_id = auto_id,
      updated = chan,
      note_links = nts[grepl(reg_xref(TRUE), nts)],
      media_links = media
    )
    
  } else if(rec_type == "REPO"){
    
    class_record_repo(
      xref = rec_xref,
      name = find_ged_values(rec_lines, "NAME"), 
      user_reference_numbers = refns, 
      notes = nts[!grepl(reg_xref(TRUE), nts)],
      address = extract_address(rec_lines),
      note_links = nts[grepl(reg_xref(TRUE), nts)],
      auto_id = auto_id,
      updated = chan
    )
    
  } else if(rec_type == "OBJE"){
    
    class_record_media(
      xref = rec_xref,
      location = find_ged_values(rec_lines, "FILE"),
      format = find_ged_values(rec_lines, c("FILE","FORM")),
      user_reference_numbers = refns,
      notes = nts[!grepl(reg_xref(TRUE), nts)],
      media_type = find_ged_values(rec_lines, c("FILE","FORM","TYPE")),
      title = find_ged_values(rec_lines, c("FILE","TITL")),
      auto_id = auto_id,
      note_links = nts[grepl(reg_xref(TRUE), nts)],
      citations = cits,
      updated = chan
    )
    
  } else if(rec_type == "NOTE"){
    
    class_record_note(
      xref = rec_xref,
      text = extract_ged_value(rec_lines[1]), 
      user_reference_numbers = refns, 
      auto_id = auto_id,
      citations = cits,
      updated = chan
    )
  }

}


extract_vals_and_types <- function(lines, val_tag){
  val_lst <- find_ged_values(lines, val_tag, return_list = TRUE)
  if(length(val_lst) == 0) return(character())
  
  vals <- sapply(val_lst, \(x) extract_ged_value(x[1]))
  types <- sapply(val_lst, \(x) {
    if(length(x) == 1) return("")
    extract_ged_value(x[2])
  })
  names(vals) <- types
  vals
}

extract_events_recorded <- function(rec_lines){
  even_lst <- find_ged_values(rec_lines, c("DATA","EVEN"), return_list = TRUE)
  if(length(even_lst) == 0) return(character())
  
  lapply(even_lst, \(x){
    class_facts_recorded(
      events = find_ged_values(x, "EVEN"),
      date_period = find_ged_values(x, c("EVEN","DATE")),
      territory = find_ged_values(x, c("EVEN","PLAC"))
    )
  })
  
}

extract_change_date <- function(rec_lines){
  change_date <- find_ged_values(rec_lines, c("CHAN","DATE"))
  if(length(change_date) == 0) return(NULL)
  
  nts <- find_ged_values(rec_lines, c("CHAN","NOTE"))
  class_change_date(
    date = toupper(change_date),
    time = find_ged_values(rec_lines, c("CHAN","DATE","TIME")),
    notes = nts[!grepl(reg_xref(TRUE), nts)],
    note_links = nts[grepl(reg_xref(TRUE), nts)]
  )
}

extract_citations <- function(lines, location = NULL){
  
  sour_lst <- find_ged_values(lines, c(location, "SOUR"), return_list = TRUE)
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
    citations = extract_citations(lines, location)
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
      call_numbers = find_ged_values(x, c("REPO","CALN"))
    )
  })
}

extract_place <- function(lines, location = NULL){
  
  place_name <- find_ged_values(lines, c(location, "PLAC"))
  if(length(place_name) == 0) return(NULL)
  
  nts <- find_ged_values(lines, c(location, "PLAC", "NOTE"))
  latlong <- paste(
    find_ged_values(lines, c(location, "PLAC", "MAP", "LATI")),
    find_ged_values(lines, c(location, "PLAC", "MAP", "LONG"))
  )
  
  class_place(
    name = place_name,
    phon_names = extract_vals_and_types(lines, c(location, "PLAC", "FONE")),
    rom_names = extract_vals_and_types(lines, c(location, "PLAC", "ROMN")),
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
    
    class_fact_fam(
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
    full = find_ged_values(lines, c(location, "ADDR")),
    city = find_ged_values(lines, c(location, "ADDR","CITY")),
    state = find_ged_values(lines, c(location, "ADDR","STAE")),
    postal_code = find_ged_values(lines, c(location, "ADDR","POST")),
    country = find_ged_values(lines, c(location, "ADDR","CTRY"))
  )
}



extract_family_links <- function(rec_lines){
  link_lst <- find_ged_values(rec_lines, "FAMS|FAMC", return_list = TRUE) 
  if(length(link_lst) == 0) return(list())
  
  lapply(link_lst, \(x){
    nts <- find_ged_values(x, c("FAMS|FAMC", "NOTE"))
    note_links <- nts[grepl(reg_xref(TRUE), nts)]
    notes <- nts[!grepl(reg_xref(TRUE), nts)]
    xref <- find_ged_values(x, "FAMC|FAMS")
    
    if(grepl("FAMC", x[1], fixed = TRUE)){
      pedi <- find_ged_values(x, c("FAMC", "PEDI"))
      
      if(length(pedi) == 0 || pedi == "birth"){
        class_child_family_link_biol(
          xref = xref,
          note_links = note_links,
          notes = notes
        )
      } else if(pedi == "adopted") {
        class_child_family_link_adop(
          xref = xref,
          note_links = note_links,
          notes = notes
        )
      } else if(pedi == "foster") {
        class_child_family_link_fost(
          xref = xref,
          note_links = note_links,
          notes = notes
        )
      } else {
        stop("Unrecognised pedigree linkage type: ", pedi)
      }
      
    } else {
      class_spouse_family_link(
        xref = xref,
        note_links = nts[grepl(reg_xref(TRUE), nts)],
        notes = nts[!grepl(reg_xref(TRUE), nts)]
      )
    }
    
  })
}

extract_notes <- function(rec_lines){
  note_lst <- find_ged_values(rec_lines, "NOTE", return_list = TRUE)
  if(length(note_lst) == 0) return(list())
  
  lapply(note_lst, \(x){
    class_note(
      text = find_ged_values(x, "NOTE"),
      language = find_ged_values(x, c("NOTE","LANG")),
      media_type = find_ged_values(x, c("NOTE","MIME")),
      translations = extract_translations(x)
    )
  })
  
  
}

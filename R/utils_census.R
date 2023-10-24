
add_census <- function(x, year, addr, xrefs_ages, roles, sour){
  
  year <- as.character(year)
  sour_xref <- x@next_xref["sour"]
  x <- push_record(x, sour)
  
  for(i in seq_along(xrefs_ages)){
    indi_rec <- pull_record(x, xrefs_ages[i])
    
    cens <- class_event_indi("CENS", date = year, address = addr)
    cens@age <- names(xrefs_ages)[i]
    
    cit <- class_citation(sour_xref = sour_xref,
                          fact_type = "CENS")
    
    if(roles[i] %in% val_roles()){
      cit@role <- roles[i]
      cit@role_phrase <- character()
    } else {
      cit@role <- "OTHER"
      cit@role_phrase <- roles[i]
    }
    
    cens@citations <- cit
    
    #is there already a census event with this year?
    existing <- overwrite <- FALSE
    for(i in seq_along(indi_rec@facts)){
      if(indi_rec@facts[[i]]@fact_type == "CENS" && indi_rec@facts[[i]]@date@as_val == year){
        existing <- TRUE
        message("Current:")
        message(paste(indi_rec@facts[[i]]@as_ged, collapse = "\n"))
        message("New:")
        message(paste(cens@as_ged, collapse = "\n"))
        overwrite <- utils::menu(c("Yes", "No"), title="Overwrite?")
        overwrite <- overwrite == 1L
        if(overwrite){
          indi_rec@facts[[i]] <- cens
        } else {
          break
        }
      }
    }
    
    if(!existing && !overwrite){
      indi_rec@facts <- append(indi_rec@facts, cens)
    }
    
    if(!existing || overwrite){
      x <- push_record(x, indi_rec)
    }
    
  }
  
  x
}


# Should it show the censuses that exists, or the censuses that are expected?
df_indi_census <- function(x, xref, max_age = 100,
                           census_years = c(seq(1841, 1921, 10), 1939)){
  
  check_indi_rec(x, xref)
  
  rec_lines <- x@indi[[xref]]
  cens_lst <- find_ged_values(rec_lines, "CENS", return_list = TRUE)
  dob <- find_ged_values(rec_lines, c("BIRT","DATE")) |> chronify()
  dob <- dob[dob != ""]
  dod <- find_ged_values(rec_lines, c("DEAT","DATE")) |> chronify()
  dod <- dod[dod != ""]
  if(length(dob) == 0 || length(dod) == 0) return(data.frame())
  
  name <- find_ged_values(rec_lines, "NAME") |> chronify()
  
  lapply(census_years, \(yr){
    # Overlap
    if(FALSE) return(data.frame())
    
    data.frame(
      xref,
      name,
      census_year = yr,
      dob,
      dod,
      age = tg$AGE,
      address = tg$ADR1,
      city = tg$CITY
    )
  }) |> 
    rbind()

  
  if(dob != ""){
    yob <- as.integer(stringr::str_extract(dob, "\\d{4}"))
  } else {
    yob <- NA_integer_
  }
  if(dod != ""){
    yod <- as.integer(stringr::str_extract(dod, "\\d{4}"))
  } else {
    yod <- NA_integer_
  }
  if(is.na(yob) & is.na(yod)) return(tibble::tibble())
  
  guess <- FALSE
  if(is.na(yob) | is.na(yod)) guess <- TRUE
  if(is.na(yob)) yob <- yod - max_age
  if(is.na(yod)) yod <- yob + max_age
  
  indi_name <- tidyged::describe_indi(tg, xref, name_only = TRUE)
  cens_years <- census_years[dplyr::between(census_years, yob, yod)]
  if(length(cens_years) == 0) return(tibble::tibble())
  
  # Do the census years appear?
  # age, adr1, city
  cens_rows <- tidyged.internals::identify_section(tg, 1, "CENS", xrefs = xref)
  if(length(cens_rows) == 0){ 
    age = "?"
    address = "?"
    city = "?"
  } else {
    tg <- dplyr::slice(tg, cens_rows) |> 
      dplyr::mutate(new_cens = tag == "CENS",
                    cens_num = cumsum(new_cens)) |>
      tidyr::pivot_wider(cens_num, names_from = tag, values_from = value) |>
      dplyr::mutate(DATE = as.integer(stringr::str_extract(DATE, "\\d{4}"))) |>
      tidyr::complete(DATE = cens_years)
  }
  
  tibble::tibble(xref = xref,
                 name = indi_name,
                 census_year = cens_years,
                 birth_year = yob,
                 death_year = yod,
                 guess = guess,
                 age = tg$AGE,
                 address = tg$ADR1,
                 city = tg$CITY)
}
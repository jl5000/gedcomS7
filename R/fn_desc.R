
desc_indi_name = function(x, xrefs, unnamed = "Unnamed individual"){

  nms <- vapply(xrefs, \(xref) {
    check_indi_rec(x, xref)
    chronify(find_ged_values(x@indi[[xref]], "NAME"))
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)
  
  nms <- gsub("/", "", nms)
  nms[nms == ""] <- unnamed
  nms
}

desc_indi_short = function(x, xrefs){
  sprintf("Individual %s, %s", xrefs, desc_indi_name(x, xrefs))
}


desc_indi_long = function(x, xref){
  
}

desc_indi_full <- function(x, xref){
  
  check_indi_rec(x, xref)
  
  indi <- pull_record(x, xref)
  indi_name <- indi@c_primary_name
  indi_sex <- indi@sex
  if(length(indi_sex) == 0 || !indi_sex %in% c("M","F")){
    pron_sub <- "They"
    pron_pos <- "Their"
  } else if (indi_sex == "M"){
    pron_sub <- "He"
    pron_pos <- "His"
  } else if (indi_sex == "F"){
    pron_sub <- "She"
    pron_pos <- "Her"
  }
  if(length(indi_name) == 0){
    indi_name <- pron_sub
  }
  
  indi_dob <- indi@c_birth_date
  indi_pob <- indi@c_birth_place
  indi_alive <- is_alive(x, xref)
  indi_dod <- indi@c_death_date
  indi_pod <- indi@c_death_place
  
  fath_xref <- get_indi_fathers(x, xref, "BIRTH")
  if(length(fath_xref) > 1) fath_xref <- fath_xref[1]
  moth_xref <- get_indi_mothers(x, xref, "BIRTH")
  if(length(moth_xref) > 1) moth_xref <- moth_xref[1]
  
  fath <- pull_record(x, fath_xref)
  fath_dob <- fath@c_birth_date
  fath_pob <- fath@c_birth_place
  fath_alive <- is_alive(x, fath_xref)
  fath_dod <- fath@c_death_date
  fath_pod <- fath@c_death_place
  
  moth <- pull_record(x, moth_xref)
  moth_dob <- moth@c_birth_date
  moth_pob <- moth@c_birth_place
  moth_alive <- is_alive(x, moth_xref)
  moth_dod <- moth@c_death_date
  moth_pod <- moth@c_death_place
  
  out <- sprintf("When %s was born", tolower(indi_name))
  if(length(indi_dob) == 1) out <- paste(out, "in", indi_dob)
  if(length(indi_pob) == 1) out <- paste(out, "in", indi_pob)
  "When {name} was born in {dob} in {pob}, 
  {sex} father, {father_name}, was {father_dob/father_dod} and
  {sex} mother, {mother_name} was {mother_dob/mother_dod}.
  
  {married} {sex} married {partner_name} in {dom} in {pom}.
  They/{sex} (have) had {num_chil} in {min(mother_dod, father_dod) - dom} years.
  
  {sex} died in {dod} in {pod} at the age of {dod - dob}."
  
  cat(out)
}

desc_fam <- function(x, xrefs){

  vapply(xrefs, \(xref) {
    check_fam_rec(x, xref)
    
    spou_nms <- get_fam_partners(x, xref) |>
      desc_indi_name(x = x)

    if(length(spou_nms) == 0){
      spou_chr <- "no individuals"
    } else {
      spou_chr <- paste(spou_nms, collapse = " and ")
    }
    
    fam_chil <- get_fam_children(x, xref)
    num_chil <- length(fam_chil)
    if(num_chil == 0) num_chil <- "no"
    if(num_chil == 1) child_plu <- "" else child_plu <- "ren"
    
    sprintf("Family %s, headed by %s, with %s child%s", xref, spou_chr, num_chil, child_plu)
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)
  
}


desc_indi_name = function(x, xrefs, unnamed = "Unnamed individual"){
  nms <- character()
  for(xref in xrefs){
    if(!is_indi_uid(x, xref)) stop("The xref is not an Individual record.")
    nm <- chronify(find_ged_values(x@indi[[xref]], "NAME"))
    nms = c(nms, nm)
  }
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
  
  "When {name} was born in {dob} in {pob}, 
  {sex} father, {father_name}, was {father_dob/father_dod} and
  {sex} mother, {mother_name} was {mother_dob/mother_dod}.
  
  {married} {sex} married {partner_name} in {dom} in {pom}.
  They/{sex} (have) had {num_chil} in {min(mother_dod, father_dod) - dom} years.
  
  {sex} died in {dod} in {pod} at the age of {dod - dob}."
  
  
}

desc_famg <- function(x, xrefs){
  spou_chr <- character()
  for(xref in xrefs){
    if(!is_fam_uid(x, xref)) stop("The xref is not an Family Group record.")
    
    spou_nms <- get_fam_partners(x, xref) |>
      desc_indi_name(unnamed = "", x = x)
    spou_nms <- spou_nms[spou_nms != ""]
    if(length(spou_nms) == 0){
      spou_chr <- c(spou_chr, "no individuals")
    } else {
      spou_chr <- c(spou_chr, paste(spou_nms, collapse = " and "))
    }
  }
  sprintf("Family %s, headed by %s", xrefs, spou_chr)
}

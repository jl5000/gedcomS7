
df_indi <- function(x){
  if(length(x@indi) == 0) return(NULL)
  
  xrefs <- names(x@indi)
  nms <- sapply(x@indi, \(lines) chronify(find_ged_values(lines, "NAME")))
  nms <- gsub("/", "", nms)
  sexes <- sapply(x@indi, \(lines) chronify(find_ged_values(lines, "SEX")))
  dobs <- sapply(x@indi, \(lines) chronify(find_ged_values(lines, c("BIRT","DATE"))))
  pobs <- sapply(x@indi, \(lines) chronify(find_ged_values(lines, c("BIRT","PLAC"))))
  alive <- sapply(x@indi, \(lines) length(grep("^1 DEAT", lines)) == 0)
  dods <- sapply(x@indi, \(lines) chronify(find_ged_values(lines, c("DEAT","DATE"))))
  pods <- sapply(x@indi, \(lines) chronify(find_ged_values(lines, c("DEAT","PLAC"))))
  moth_xref <- sapply(xrefs, \(xref) chronify(get_indi_mothers(x, xref, TRUE)))
  moth_nm <- sapply(moth_xref, \(xref) if(xref == "") "" else chronify(find_ged_values(x@indi[[xref]], "NAME")))
  moth_nm <- gsub("/", "", moth_nm)
  fath_xref <- sapply(xrefs, \(xref) chronify(get_indi_fathers(x, xref, TRUE)))
  fath_nm <- sapply(fath_xref, \(xref) if(xref == "") "" else chronify(find_ged_values(x@indi[[xref]], "NAME")))
  fath_nm <- gsub("/", "", fath_nm)
  chan_dt <- sapply(x@indi, \(lines) chronify(find_ged_values(lines, c("CHAN","DATE"))))
  
  data.frame(
    xref = xrefs,
    name = nms,
    sex = sexes,
    birth_date = dobs,
    birth_place = pobs,
    is_alive = alive,
    death_date = dods,
    death_place = pods,
    mother_xref = moth_xref,
    mother_name = moth_nm,
    father_xref = fath_xref,
    father_name = fath_nm,
    last_modified = chan_dt
  )
  
}

df_famg <- function(x){
  if(length(x@famg) == 0) return(NULL)
  
}

df_sour <- function(x){
  if(length(x@sour) == 0) return(NULL)
  
}

df_repo <- function(x){
  if(length(x@repo) == 0) return(NULL)
  
}

df_media <- function(x){
  if(length(x@media) == 0) return(NULL)
  
}

df_note <- function(x){
  if(length(x@note) == 0) return(NULL)
  
}

df_indi_facts <- function(x, xref){
  if(!is_indi_xref(x, xref)) stop("The xref is not for an Individual record.")
  
  
}

df_famg_facts <- function(x, xref){
  if(!is_fam_uid(x, xref)) stop("The xref is not for a Family record.")
  
}


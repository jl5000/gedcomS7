
to_console <- function(label, val, exdent){
  if(length(val) == 0) return()
  cat(strwrap(val, 
              initial = sprintf(paste0("%-", exdent, "s"), label), 
              prefix = "", 
              exdent = exdent), 
      fill = TRUE)
}

R7::method(print, class_gedcomR7) <- function(x, ...){
  
  eol <- "\n"
  exdent <- 24 # nchar("Source system version:") + 2 = 24
  
  cat("GEDCOM file summary:")
  cat(eol, eol)
  to_console("Submitter:", x@subm@name, exdent)
  to_console("Description:", x@content_description, exdent)
  to_console("Language:", x@language, exdent)
  to_console("Encoding:", x@character_encoding, exdent)
  cat(eol)
  to_console("Copyright:", x@gedcom_copyright, exdent)
  cat(eol)
  to_console("Source system:", x@system_id, exdent)
  to_console("Source system version:", x@product_version, exdent)
  to_console("Product name:", x@product_name, exdent)
  to_console("Product source:", x@business_name, exdent)
  cat(eol)
  to_console("Individuals:", length(x@indi), exdent)
  to_console("Families:", length(x@famg), exdent)
  to_console("Sources:", length(x@sour), exdent)
  to_console("Repositories:", length(x@repo), exdent)
  to_console("Multimedia:", length(x@media), exdent)
  to_console("Notes:", length(x@note), exdent)

}


R7::method(print, class_record_indi) <- function(x, ...){

  eol <- "\n"
  exdent <- 32 # nchar("10 Other individual attribute:") + 2 = 32
  
  to_console("xref:", x@xref, exdent)
  to_console("Name(s):", paste(x@all_names, collapse = ", "), exdent)
  to_console("Sex:", names(which(val_sexes() == x@sex)), exdent)
  for(i in seq_along(x@user_reference_numbers)){
    num <- x@user_reference_numbers[i]
    typ <- names(x@user_reference_numbers)[i]
    if(typ != "") typ <- sprintf(" (%s)", typ) 
    to_console("User ref:", paste0(num, typ), exdent)
  }
  cat(eol)
  
  i <- 0
  for(fac in x@facts){
    i <- i + 1
    lbl <- sprintf("%s %s:", i, names(which(c(val_attribute_types(),
                                        val_individual_event_types()) == fac@fact)))
    dat <- fac@fact_date
    if(length(dat) == 1) dat <- sprintf("(%s)", dat)
    age <- fac@age
    if(length(age) == 1) age <- sprintf("(%s)", age)
    to_console(lbl, paste(fac@description, dat, fac@fact_location, age), exdent)
  }
  cat(eol)
  
  for(nt in x@notes){
    nt_len <- 100
    if(nchar(nt) > nt_len){
      nt <- paste0(substr(nt, 1, nt_len), "...")
    }
    to_console("Note:", nt, exdent)
  }

}
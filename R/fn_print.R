
to_console <- function(label, val, exdent){
  if(length(val) == 0) val <- "<Undefined>"
  cat(strwrap(val, 
              initial = sprintf(paste0("%-", exdent, "s"), label), 
              prefix = "", 
              exdent = exdent), 
      fill = TRUE)
}


S7::method(print, GedcomS7) <- function(x, ...){
  
  eol <- "\n"
  exdent <- 24 # nchar("Source system version:") + 2 = 24
  
  cat("GEDCOM file summary:")
  cat(eol, eol)
  to_console("GEDCOM version:", x@gedcom_version, exdent)
  to_console("Creation Date:", obj_to_val(x@creation_date), exdent)
  to_console("Default Language:", x@default_language, exdent)
  if(length(x@subm_xref) == 0){
    subm <- x@subm_xref
  } else {
    subm <- find_ged_values(x@subm[[x@subm_xref]], "NAME")
  }
  to_console("Submitter:", subm, exdent)
  cat(eol)
  to_console("Copyright:", x@gedcom_copyright, exdent)
  cat(eol)
  
  if(length(x@source) == 0){
    to_console("Source system:", x@source, exdent)
  } else {
    to_console("Source system:", x@source@product_id, exdent)
    to_console("Product name:", x@source@product_name, exdent)
    to_console("Source system version:", x@source@product_version, exdent)
  }
 
  cat(eol)
  to_console("Submitters:", length(x@records@RAW@SUBM), exdent)
  to_console("Individuals:", length(x@records@RAW@INDI), exdent)
  to_console("Families:", length(x@records@RAW@FAM), exdent)
  to_console("Sources:", length(x@records@RAW@SOUR), exdent)
  to_console("Repositories:", length(x@records@RAW@REPO), exdent)
  to_console("Multimedia:", length(x@records@RAW@OBJE), exdent)
  to_console("Notes:", length(x@records@RAW@SNOTE), exdent)

}


S7::method(print, Record) <- function(x, ...){
  str(x, max.level = 1)
}

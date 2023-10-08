
to_console <- function(label, val, exdent){
  if(length(val) == 0) return()
  cat(strwrap(val, 
              initial = sprintf(paste0("%-", exdent, "s"), label), 
              prefix = "", 
              exdent = exdent), 
      fill = TRUE)
}


S7::method(print, class_gedcomS7) <- function(x, ...){
  
  eol <- "\n"
  exdent <- 24 # nchar("Source system version:") + 2 = 24
  
  cat("GEDCOM file summary:")
  cat(eol, eol)
  # to_console("Submitter:", x@subm@name, exdent)
  # to_console("Language:", x@default_language, exdent)
  cat(eol)
  to_console("Copyright:", x@gedcom_copyright, exdent)
  cat(eol)
  # to_console("Source system:", x@source@product_id, exdent)
  # to_console("Source system version:", x@source@product_version, exdent)
  # to_console("Product name:", x@source@product_name, exdent)
  # to_console("Product source:", x@source@business_name, exdent)
  cat(eol)
  to_console("Submitters:", length(x@subm), exdent)
  to_console("Individuals:", length(x@indi), exdent)
  to_console("Families:", length(x@fam), exdent)
  to_console("Sources:", length(x@sour), exdent)
  to_console("Repositories:", length(x@repo), exdent)
  to_console("Multimedia:", length(x@media), exdent)
  to_console("Notes:", length(x@note), exdent)

}


S7::method(print, class_record) <- function(x, ...){
  x@as_ged
}

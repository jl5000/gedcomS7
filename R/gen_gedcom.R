
raw_header_summary <- function(hd){
  exdent <- 24
  to_console("GEDCOM version:", hd@gedcom_version, exdent)
  to_console("Creation Date:", obj_to_val(hd@creation_date), exdent)
  to_console("Default Language:", hd@default_language, exdent)
  cat("\n")
  to_console("Copyright:", hd@gedcom_copyright, exdent)
  cat("\n")
  raw_source_summary(hd@source)

}

raw_source_summary <- function(sour){
  exdent <- 24
  if(length(sour) == 0){
    to_console("Source system:", sour, exdent)
  } else {
    to_console("Source system:", sour@product_id, exdent)
    to_console("Product name:", sour@product_name, exdent)
    to_console("Source system version:", sour@product_version, exdent)
  }
}

raw_record_summary <- function(raw){
  exdent <- 24
  to_console("Submitters:", length(raw@SUBM), exdent)
  to_console("Individuals:", length(raw@INDI), exdent)
  to_console("Families:", length(raw@FAM), exdent)
  to_console("Sources:", length(raw@SOUR), exdent)
  to_console("Repositories:", length(raw@REPO), exdent)
  to_console("Multimedia:", length(raw@OBJE), exdent)
  to_console("Notes:", length(raw@SNOTE), exdent)
}



# Source ------------------------------------------------------------------

S7::method(print, GedcomSource) <- function(x, ...){
  str(x, max.level = 1)
}
S7::method(summary, GedcomSource) <- function(x, ...){
  raw_source_summary(x)
}

# Header ------------------------------------------------------------------

S7::method(print, GedcomHeader) <- function(x, ...){
  summary(x)
}
S7::method(summary, GedcomHeader) <- function(x, ...){
  raw_header_summary(x)
}

# Raw ---------------------------------------------------------------------

S7::method(print, GedcomRecordsRaw) <- function(x, ...){
  summary(x)
}
S7::method(summary, GedcomRecordsRaw) <- function(x, ...){
  raw_record_summary(x)
}

# Records -----------------------------------------------------------------

S7::method(print, GedcomRecords) <- function(x, ...){
  summary(x@RAW)
}
S7::method(summary, GedcomRecords) <- function(x, ...){
  raw_record_summary(x@RAW)
}

# Gedcom ------------------------------------------------------------------

S7::method(print, GedcomS7) <- function(x, ...){
  summary(x)
}

S7::method(summary, GedcomS7) <- function(x, ...){
  exdent <- 24 # nchar("Source system version:") + 2 = 24
  
  cat("GEDCOM file summary:")
  cat("\n", "\n")
  raw_header_summary(x@header)
  cat("\n")
  raw_record_summary(x@records@RAW)
}


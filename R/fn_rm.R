


rm_records <- function(x, xrefs){
  xrefs <- unique(xrefs)
  for(xref in xrefs){
    for(rec_type in val_record_types()){
      
      S7::prop(x, rec_type)[[xref]] <- NULL
      
      S7::prop(x, rec_type) <- lapply(S7::prop(x, rec_type), 
                                      \(lines) replace_xref_values(lines, xref))
    }
  }
  x
}


replace_xref_values <- function(lines, xref){
  rows <- extract_ged_value(lines) == xref
  if(sum(rows) == 0) return(lines)
  lines[rows] <- sub(paste0(xref, "$"), "@VOID@", lines[rows])
  lines
}
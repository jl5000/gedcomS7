
fix_maximal_header <- function(maximal_path){
  ged <- readLines(maximal_path)
  
  # Fix NOTE in header
  move <- ged[4:12]
  ged <- append(ged, move, 49)
  ged <- ged[-(4:12)]
  ged
}

fix_maximal_records <- function(maximal_path){
  ged <- readLines(maximal_path)
  
  # Fix CHR.AGE in @I1@
  move <- ged[387:388]
  ged <- append(ged, move, 385) 
  ged <- ged[-(389:390)]
  ged
}

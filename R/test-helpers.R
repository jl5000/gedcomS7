
test_ged <- function(){
  
  indi <- IndividualRecord(pers_names = "Joe /Bloggs/", sex = "M",
                           facts = IndividualEvent("BIRT", date = "20 MAR 1967"))
  suppressMessages(
    new_gedcom() |> 
      push_record(indi) |> 
      add_parents("@I1@", moth_name = "Mother /Bloggs/", 
                  fath_name = "Father /Bloggs/") |> 
      add_spouse("@I1@", sex = "F", spou_name = "Jess /Bloggs/") |> 
      add_children("@F2@", sexes = "MF", chil_names = c("Son","Daughter"))
  )
  
  
}

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
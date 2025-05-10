
test_ged <- function(){
  
  indi <- IndividualRecord(
    pers_names = "Joe /Bloggs/", 
    sex = "M",
    facts = list(
      IndividualEvent("BIRT", date = "20 MAR 1967",
                      place = "California, USA"),
      IndividualAttribute("DSCR", 
                          fact_val = "5 ft 10, brown hair, brown eyes"),
      IndividualEvent("DEAT", date = "8 APR 2018",
                      place = "Florida, USA",
                      cause = "Pneumonia",
                      age = "51y"),
      IndividualAttribute("FACT", fact_val = "One arm", fact_desc = "Disability")
    )
    
  )
  
  
  suppressMessages({
    ged <- new_gedcom() |> 
      push_record(indi) |> 
      add_parents("@I1@", moth_name = "Mother /Bloggs/", 
                  fath_name = "Father /Bloggs/") |> 
      add_spouse("@I1@", sex = "F", spou_name = "Jess /Bloggs/") |> 
      add_children("@F2@", sexes = "MF", chil_names = c("Son","Daughter")) |> 
      push_record(FamilyRecord(unique_ids = "f511d543-43c2-4642-b7dd-31c1a2a6bbc2",
                               user_ids = c("My ID" = "1234"),
                               ext_ids = c("http://www.website.com" = "page1"))) |> # unused family
      push_record(SubmitterRecord(subm_name = "Submitter 1",
                                  address = "A road")) |> 
      push_record(SubmitterRecord(subm_name = "Submitter 2",
                                  address = "Another road")) |> 
      push_record(NoteRecord(text = "This is a note", language = "en")) |> 
      push_record(NoteRecord(text = paste(rep_len("a", 51), collapse = ""), 
                             language = "es"))
    
    ged@header@subm_xref <- "@U1@"
    spouse <- pull_record(ged, "@I4@")
    spouse@facts <- IndividualEvent("CHRA", date = "1998", note_xrefs = "@N2@")
    ged <- push_record(ged, spouse)
    parents <- pull_record(ged, "@F1@")
    parents@facts <- FamilyEvent("MARR", date = "MAR 1965",
                                 place = "London, England",
                                 husb_age = "28y", wife_age = "25y")
    ged <- push_record(ged, parents)
  })
  
  ged
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
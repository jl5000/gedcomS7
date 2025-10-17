
as_ged <- S7::new_generic("as_ged", "x")

S7::method(as_ged, NULL) <- function(x){character()}
S7::method(as_ged, GedcomS7class) <- function(x, ...){x@GEDCOM}
S7::method(as_ged, S7::class_list) <- function(x){
  if(length(x) == 0) return(character())
  unlist(lapply(x, \(y) as_ged(y)))
}
S7::method(as_ged, S7::class_atomic) <- function(x, tag1 = NULL, tag2 = NULL){
  stopifnot("Object contains atomic elements - a tag is required" = !is.null(tag1))
  if(length(x) == 0) return(character())
  if(is.null(tag2)) return(paste(0, tag1, x))

  # named vector
  ged <- character()
  for(i in seq_along(x)){
    ged <- c(
      ged,
      paste(0, tag1, x[i]),
      paste(1, tag2, names(x)[i])
    )
  }
  ged <- ged[ged != paste(1, tag2, "")]
  ged
}

as_val <- S7::new_generic("as_val", "x")
S7::method(as_val, NULL) <- function(x){x}
S7::method(as_val, S7::class_character) <- function(x){x}
S7::method(as_val, GedcomS7class) <- function(x){x@GEDCOM_STRING}

restrictions_to_resn <- function(confidential, locked, private){
  if(!any(confidential, locked, private))
    return(character())
  
  conf <- rep("CONFIDENTIAL", confidential)
  lock <- rep("LOCKED", locked)
  priv <- rep("PRIVACY", private)
  
  toString(c(conf, lock, priv))
}

identifiers_to_ged <- function(user_ids, unique_ids, ext_ids){
  c(
    as_ged(user_ids, "REFN", "TYPE"),
    sprintf("0 UID %s", unique_ids),
    as_ged(ext_ids, "EXID", "TYPE")
  )
}

notes_to_ged <- function(notes, note_xrefs){
  c(
    as_ged(notes),
    sprintf("0 SNOTE %s", note_xrefs)
  )
}

contacts_to_ged <- function(address, phone_numbers, emails, faxes, web_pages){
  c(
    as_ged(address),
    sprintf("0 PHON %s", phone_numbers),
    sprintf("0 EMAIL %s", emails),
    sprintf("0 FAX %s", faxes),
    sprintf("0 WWW %s", web_pages)
  )
}

as.S7class_list <- function(input, S7class){
  if("S7_object" %in% class(input)) input <- list(input)
  lapply(input, \(x) 
         if(is.atomic(x)){
           do.call(S7class, list(x))
         } else {
           if(S7::S7_inherits(x, S7class)){
             x
           } else {
             sprintf("contains an invalid object not of class %s.", 
                     S7class@name)
           }
         }
  )
}

as.S7class <- function(input, S7class){
  if(length(input) == 0) return(input)
  if(is.atomic(input)) input <- do.call(S7class, list(input))
  input
}

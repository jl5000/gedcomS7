
# as_ged <- S7::new_generic("as_ged", "x")
# 
# S7::method(as_ged, NULL) <- function(x){character()}
# S7::method(as_ged, GedcomS7class) <- function(x){x@GEDCOM}
# 
# S7::method(as_ged, S7::class_vector) <- function(x, tag1 = NULL, tag2 = NULL){
#   stopifnot("Object contains atomic elements - a tag is required" = !is.null(tag1))
#   if(length(x) == 0) return(character())
#   if(is.null(tag2)) return(paste(0, tag1, x))
#     
#   # named vector
#   ged <- character()
#   for(i in seq_along(x)){
#     ged <- c(
#       ged,
#       paste(0, tag1, x[i]),
#       paste(1, tag2, names(x)[i])
#     )
#   }
#   ged <- ged[ged != paste(1, tag2, "")]
#   ged
# }
# S7::method(as_ged, S7::class_list) <- function(x){
#   out <- character()
#   for(input in x){ # all elements will be S7 objects
#     out <- c(out, as_ged(input))
#   }
#   out
# }


#' Convert an input into a vector of GEDCOM lines
#'
#' @param obj Either an atomic vector, S7 class object, or list.
#' Any S7 class objects must have an `GEDCOM()` method.
#' @param tag If the obj contains any atomic elements, then this
#' will specify what tag they are recorded against.
#'
#' @returns A vector of GEDCOM lines.
#' @keywords internal
obj_to_ged <- function(obj, tag = NULL){
  
  if(is.atomic(obj) && is.null(tag))
    stop("Object contains atomic elements - a tag is required")
  
  if(is.atomic(obj)) return(sprintf("0 %s %s", tag, obj))
  
  if("S7_object" %in% class(obj)) return(obj@GEDCOM)
  
  if(length(obj) == 0) return(character())
  
  out = character()
  for(input in obj){
    out <- c(out, obj_to_ged(input, tag))
  }
  
  out
}


named_vec_to_ged <- function(vec, tag1, tag2){
  ged <- character()
  for(i in seq_along(vec)){
    ged <- c(
      ged,
      sprintf("0 %s %s", tag1, vec[i]),
      sprintf("1 %s %s", tag2, names(vec)[i])
    )
  }
  ged <- ged[ged != sprintf("1 %s ", tag2)]
  ged
}



obj_to_val <- function(obj){
  if("S7_object" %in% class(obj)){
    val <- obj@GEDCOM_STRING
  } else {
    val <- obj # character/NULL
  }
  val
}

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
    named_vec_to_ged(user_ids, "REFN", "TYPE"),
    sprintf("0 UID %s", unique_ids),
    named_vec_to_ged(ext_ids, "EXID", "TYPE")
  )
}

notes_to_ged <- function(notes, note_xrefs){
  c(
    obj_to_ged(notes),
    sprintf("0 SNOTE %s", note_xrefs)
  )
}

contacts_to_ged <- function(address, phone_numbers, emails, faxes, web_pages){
  c(
    obj_to_ged(address),
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

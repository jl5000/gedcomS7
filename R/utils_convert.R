
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

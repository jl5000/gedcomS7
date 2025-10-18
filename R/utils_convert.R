
as_ged <- S7::new_generic("as_ged", "x")

S7::method(as_ged, NULL) <- function(x) character()
# ... for DATE/TIME where it could be character or S7 object
S7::method(as_ged, GedcomS7class) <- function(x, ...) x@GEDCOM
S7::method(as_ged, S7::class_list) <- function(x){
  if(length(x) == 0) return(character())
  unlist(lapply(x, \(y) as_ged(y)))
}
S7::method(as_ged, S7::class_atomic) <- function(x, tags){
  stopifnot("One or two tags must be supplied" = length(tags) %in% 1:2)
  if(length(x) == 0) return(character())
  if(length(tags) == 1) return(paste(0, tags, x))
  if(is.null(names(x))) return(paste(0, tags[1], x))

  # named vector
  ged <- mapply(\(i, j) c(paste(0, tags[1], i), paste(1, tags[2], j)),
                x, names(x), SIMPLIFY = TRUE, USE.NAMES = FALSE)
  ged <- ged[ged != paste(1, tags[2], "")]
  ged
}

as_val <- S7::new_generic("as_val", "x")
S7::method(as_val, NULL) <- function(x) x
S7::method(as_val, S7::class_character) <- function(x) x
S7::method(as_val, GedcomS7class) <- function(x) x@GEDCOM_STRING


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

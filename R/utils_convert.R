
as_ged <- S7::new_generic("as_ged", "x")

S7::method(as_ged, NULL) <- function(x, ...) character()
# ... for DATE/TIME where it could be character or S7 object
S7::method(as_ged, GedcomS7class) <- function(x, tags = NULL, lvl = 0){
  level_up(x@GEDCOM, lvl)
} 
S7::method(as_ged, S7::class_list) <- function(x, lvl = 0){
  unlist(lapply(x, \(y) as_ged(y, lvl = lvl))) %||% character()
}
S7::method(as_ged, S7::class_atomic) <- function(x, tags, lvl = 0){
  stopifnot("One or two tags must be supplied" = length(tags) %in% 1:2)
  if(length(x) == 0) return(character())
  if(length(tags) == 1) return(paste(lvl, tags, x))
  if(is.null(names(x))) return(paste(lvl, tags[1], x))

  # named vector
  ged <- mapply(\(i, j) c(paste(lvl, tags[1], i), paste(lvl + 1, tags[2], j)),
                x, names(x), SIMPLIFY = TRUE, USE.NAMES = FALSE)
  ged[ged != paste(lvl + 1, tags[2], "")]
}



as_val <- S7::new_generic("as_val", "x")
S7::method(as_val, NULL) <- function(x) x
S7::method(as_val, S7::class_character) <- function(x) x
S7::method(as_val, GedcomS7class) <- function(x) x@GEDCOM_STRING



as.S7class_list <- S7::new_generic("as.S7class_list", "x")
S7::method(as.S7class_list, GedcomS7class) <- function(x, S7class){
  if(!paste0("gedcomS7::", S7class@name) %in% class(x)) 
    stop(sprintf("%s cannot be converted to class %s.", class(x)[1], S7class@name))
  
  list(x)
}
S7::method(as.S7class_list, S7::class_list) <- function(x, S7class){
  do.call(c, lapply(x, as.S7class_list, S7class)) %||% x
}
S7::method(as.S7class_list, S7::class_atomic) <- function(x, S7class){
  lapply(x, \(i) do.call(S7class, list(i)))
} 



as.S7class <- S7::new_generic("as.S7class", "x")
S7::method(as.S7class, NULL) <- function(x, S7class) x
S7::method(as.S7class, GedcomS7class) <- function(x, S7class) x # Assume developer gives it correct class!
S7::method(as.S7class, S7::class_atomic) <- function(x, S7class){
  if(length(x) == 0) return(x)
  do.call(S7class, list(x))
} 


S7::method(print, Address) <- function(x, ...){
  summary(x)
}

S7::method(summary, Address) <- function(x, ...){
  cat(x@GEDCOM_STRING)
}
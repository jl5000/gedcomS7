

S7::method(print, DateClass) <- function(x, ...){
  summary(x)
}
S7::method(summary, DateClass) <- function(x, ...){
  date_str <- x@GEDCOM_STRING
  
  if(S7::S7_inherits(x, DateSorting) || S7::S7_inherits(x, DateValue)){
    if(length(x@time) == 1) date_str <- paste(date_str, obj_to_val(x@time))
    if(length(x@date_phrase) == 1) date_str <- sprintf("%s (%s)",
                                                       date_str, x@date_phrase)
  }
  
  cat(date_str)
}



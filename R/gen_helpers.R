
to_console <- function(label, val, exdent){
  if(length(val) == 0) val <- "<Undefined>"
  cat(strwrap(val, 
              initial = sprintf(paste0("%-", exdent, "s"), label), 
              prefix = "", 
              exdent = exdent), 
      fill = TRUE)
}


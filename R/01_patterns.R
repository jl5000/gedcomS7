
`@` <- R7::`@`

anchor_it <- function(reg) {
  paste0("^", reg, "$")
}
reg_xref <- function(only = TRUE) {
  #p31
  reg <- "@[a-zA-Z0-9]{1,20}@"
  if(only) reg <- anchor_it(reg)
  reg
}

reg_age_at_event <- function() {
  paste0("^(?:[<>] )?",
         c("\\d{1,3}y \\d{1,2}m \\d{1,3}d$",
           "\\d{1,3}y \\d{1,2}m$",
           "\\d{1,3}y \\d{1,3}d$",
           "\\d{1,2}m \\d{1,3}d$",
           "\\d{1,3}y$",
           "\\d{1,2}m$",
           "\\d{1,3}d$")) |> 
    paste(collapse = "|")
  
}
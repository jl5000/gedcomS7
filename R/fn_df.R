
df_indi <- function(x){
  
  
}

df_famg<- function(x){
  
  
}

df_sour <- function(x){
  
  
}

df_repo <- function(x){
  
  
}

df_media <- function(x){
  
  
}

df_note <- function(x){
  
  
}

df_indi_facts <- function(x, xref){
  if(!is_indi_xref(x, xref)) stop("The xref is not an Individual record.")
  
  
}

df_famg_facts <- function(x, xref){
  if(!is_famg_xref(x, xref)) stop("The xref is not a Family Group record.")
  
}

# df_indi = R7::new_property(
#   R7::class_data.frame,
#   getter = function(self){
#     if(length(self@indi) == 0) return(NULL)
#     data.frame(
#       xref = names(self@indi),
#       name = apply_extract_ged_values(self@indi, "NAME"),
#       sex = apply_extract_ged_values(self@indi, "SEX"),
#       birth_date = apply_extract_ged_values(self@indi, c("BIRT","DATE")),
#       birth_place = "",#pop(ind@birth_place),
#       is_alive = "",#ind@is_alive,
#       death_date = apply_extract_ged_values(self@indi, c("DEAT","DATE")),
#       death_place = ""#pop(ind@death_place)
#     )
    # lapply(
    #   self@indi,
    #   function(ind){
    #     data.table::data.table(
    #       xref = ind@xref,
    #       name = pop(ind@primary_name),
    #       sex = pop(ind@sex),
    #       birth_date = pop(ind@birth_date),
    #       birth_place = pop(ind@birth_place),
    #       is_alive = ind@is_alive,
    #       death_date = pop(ind@death_date),
    #       death_place = pop(ind@death_place)
    #     )
    #   }) |>
    #   data.table::rbindlist()
  # }),

# df_famg = R7::new_property(
#   R7::class_data.frame,
#   getter = function(self){
#     if(length(self@famg) == 0) return(NULL)
#     lapply(
#       self@famg,
#       function(fam){
#         
#         husb_name <- ""
#         wife_name <- ""
#         if(length(fam@husb_xref) == 1)
#           husb_name <- self@indi[[fam@husb_xref]]@primary_name
#         if(length(fam@wife_xref) == 1)
#           wife_name <- self@indi[[fam@wife_xref]]@primary_name
#         num_chil <- 0
#         if(length(fam@num_children) == 1)
#           num_chil <- fam@num_children
#         
#         data.table::data.table(
#           xref = fam@xref,
#           husband = pop(husb_name),
#           wife = pop(wife_name),
#           relationship_date = pop(fam@relationship_date),
#           relationship_place = pop(fam@relationship_place),
#           num_children = max(length(c(fam@chil_biol_xref,fam@chil_adop_xref,fam@chil_fost_xref)), num_chil)
#         )
#       }) |>
#       data.table::rbindlist()
#   }),
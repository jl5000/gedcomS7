
`@` <- R7::`@`
.datatable.aware = TRUE

pop <- function(x){
  paste(x, collapse = "")
}

df_rows <- function(level, tag, value, record = NULL){
  if(length(value) == 0) return(NULL)
  df <- data.table::data.table(level = level, tag = tag, value = value)
  if(!is.null(record)){
    df[,record:=record]
    data.table::setcolorder(df, c(4,1,2,3))
  }
  df[]
}

lst_to_df <- function(lst, level_inc = 0){
  if(length(lst) == 0) return(NULL)
  
  lst_df <- lapply(lst, `@`, as_df) |>
    data.table::rbindlist()
  
  if(lst_df[,.N] == 0) return(NULL)
  
  lst_df$level <- lst_df$level + level_inc
  
  lst_df
}

obj_to_df <- function(obj, level_inc = 0){
  if(is.null(obj)) return(NULL)
  df <- obj@as_df
  if(is.null(df)) return(NULL)
  df$level = df$level + level_inc
  df
}

date_to_df <- function(obj, level_inc = 0){
  if(is.null(obj)) return(NULL)
  if(is.character(obj)){
    date_val <- obj
  } else {
    date_val <- obj@as_val
  }
  df_rows(level = level_inc, tag = "DATE", value = date_val)
}

get_valid_xref <- function(x, xref, type){
  if (length(xref) == 0) xref <- x@active_record
  if (length(xref) == 0)
    stop("No xref is provided and no record is activated.")
  if(!xref %in% x@xrefs[[type]])
    stop(sprintf("Appropriate record with xref %s not found.", xref))
  xref
}

life_story <- function(sex = NULL,
                       name = NULL,
                       dob = NULL,
                       pob = NULL,
                       mother_name = NULL,
                       father_name = NULL,
                       mother_dob = NULL,
                       mother_dod = NULL,
                       father_dob = NULL,
                       father_dod = NULL,
                       partner_name = NULL,
                       partner_sex = NULL,
                       married = NULL,
                       num_chil = NULL,
                       dom = NULL,
                       pom = NULL,
                       dod = NULL,
                       pod = NULL){
  
  "When {name} was born in {dob} in {pob}, 
  {sex} father, {father_name}, was {father_dob/father_dod} and
  {sex} mother, {mother_name} was {mother_dob/mother_dod}.
  
  {married} {sex} married {partner_name} in {dom} in {pom}.
  They/{sex} (have) had {num_chil} in {min(mother_dod, father_dod) - dom} years.
  
  {sex} died in {dod} in {pod} at the age of {dod - dob}."
  
  
}
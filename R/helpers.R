
`@` <- R7::`@`

extract_ged_level <- function(lines){
  as.integer(sub(reg_ged_line(), "\\1", lines))
}
extract_ged_xref <- function(lines){
  sub(reg_ged_line(), "\\2", lines)
}
extract_ged_tag <- function(lines){
  sub(reg_ged_line(), "\\3", lines)
}
extract_ged_value <- function(lines){
  sub(reg_ged_line(), "\\4", lines)
}

delete_ged_section <- function(lines, line_no){
  lvl <- as.integer(substr(lines[line_no], 1, 1))
  lines <- lines[-line_no]
  while(line_no <= length(lines) && 
        as.integer(substr(lines[line_no], 1, 1)) > lvl){
    lines <- lines[-line_no]
  }
  lines
}

find_ged_values <- function(lines, 
                               tag,
                               return_list = FALSE){
  
  base_level <- extract_ged_level(lines[1]) - 1
  
  # Ignore parent if lines describes a whole record
  if(extract_ged_xref(lines[1]) != ""){
    lines <- lines[-1]
    base_level <- base_level + 1
  }
  
  if(length(tag) > length(lines)) return(character())
  
  for(level in seq_along(tag)){
    
    lines_lst <- split(lines, cumsum(extract_ged_level(lines) == base_level + level))
    
    lines_lst <- Filter(\(x) grepl(sprintf("^%s (%s)( (?s).*)?$", base_level + level, tag[level]), x[1], perl = TRUE), 
                        lines_lst)
    
    if(level == length(tag)){ # final tag
      if(return_list){
        return(unname(lines_lst))
      } else {
        # strip out subordinates
        lines_lst <- lapply(lines_lst, `[`, 1)
      }
    } else { # remove parent tag ready for splitting again
      lines_lst <- lapply(lines_lst, `[`, -1)
    }
    
    if(length(lines_lst) == 0) return(character())
    
    lines <- unlist(lines_lst)
  }
  
  lines <- unname(lines)
  # Catch cases where no line value is given
  lines <- lines[lines != paste(base_level + length(tag), tag[length(tag)])]
  sub(sprintf("^%s (%s) ((?s).*)$", base_level + length(tag), tag[length(tag)]), "\\2", lines, perl = TRUE)
}


apply_extract_ged_values <- function(recs, tag){
  
  recs |>
    lapply(\(x) find_ged_values(x, tag)) |>
    lapply(\(x) if(length(x) == 0) pop(x) else x[1]) |>
    unlist() |>
    unname()
  
}


pop <- function(x){
  paste(x, collapse = "")
}

increase_level <- function(ged, by = 1){
  if(length(ged) == 0) return(character())
  
  cur_level <- as.integer(substr(ged, 1, 1))
  remainder <- substr(ged, 3, nchar(ged))
  paste(cur_level + by, remainder)
}


lst_to_ged <- function(lst){
  if(length(lst) == 0) return(character())
  
  lapply(lst, `@`, as_ged) |>
    unlist()
}

obj_to_ged <- function(obj){
  if(is.null(obj)) return(character())
  obj@as_ged
}

date_to_val <- function(obj){
  if(R7::R7_inherits(obj, class_date)){
    date_val <- obj@as_val
  } else {
    date_val <- obj
  }
  date_val
}


get_record_type <- function(record){
  
  if(R7::R7_inherits(record, class_record_indi)){
    "indi"
  } else if(R7::R7_inherits(record, class_record_famg)){
    "famg"
  } else if(R7::R7_inherits(record, class_record_sour)){
    "sour"
  } else if(R7::R7_inherits(record, class_record_repo)){
    "repo"
  } else if(R7::R7_inherits(record, class_record_media)){
    "media"
  } else if(R7::R7_inherits(record, class_record_note)){
    "note"
  } else {
    stop("Unrecognised record")
  }
  
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

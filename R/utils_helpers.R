#' @include utils_at.R
NULL

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
  vals <- extract_ged_value(lines)
  vals[vals != ""]
  
  # lines <- lines[lines != paste(base_level + length(tag), tag[length(tag)])]
  # sub(sprintf("^%s (%s) ((?s).*)$", base_level + length(tag), tag[length(tag)]), "\\2", lines, perl = TRUE)
}


#' Force a vector to be a length 1 character vector
#' 
#' The name comes from 'Chr-one-ify'.
#'
#' @param x An atomic vector of any length.
#'
#' @return A character vector of length one. It is either an empty string for a
#' zero length input, or takes the value of the first element.
chronify <- function(x){
  if(length(x) == 0) return("")
  as.character(x)[1]
}

#' Increase the level of a vector of GEDCOM lines
#'
#' @param ged A character vector of GEDCOM lines.
#' @param by The number of levels to increment.
#'
#' @return The vector of GEDCOM lines with incremented levels.
increase_level <- function(ged, by = 1){
  if(length(ged) == 0) return(character())
  
  cur_level <- extract_ged_level(ged)
  remainder <- substr(ged, 3, nchar(ged))
  paste(cur_level + by, remainder)
}


#' Convert an input into a vector of GEDCOM lines
#'
#' @param obj Either an atomic vector, S7 class object, or list.
#' Any S7 class objects must have an `as_ged()` method.
#' @param tag If the obj contains any atomic elements, then this
#' will specify what tag they are recorded against.
#'
#' @return
obj_to_ged <- function(obj, tag = NULL){

  if(length(obj) == 0) {
    
    return(character())
    
  } else {
    if(is.atomic(obj)){
      if(is.null(tag)) stop("Object contains atomic elements - a tag is required")
      return(sprintf("0 %s %s", tag, obj))
      
    } else if("S7_object" %in% class(obj)){
      
      return(obj@as_ged)
      
    } else if(is.list(obj)){
      
      out = character()
      for(input in obj){
        out <- c(out, obj_to_ged(input, tag))
      }
      return(out)
    }
    
  }
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


datetime_to_val <- function(obj){
  if(S7::S7_inherits(obj, class_date) | 
     S7::S7_inherits(obj, class_time)){
    date_val <- obj@as_val
  } else {
    date_val <- obj # character/NULL
  }
  date_val
}



get_record_type <- function(record){
  
  if(S7::S7_inherits(record, class_record_indi)){
    "indi"
  } else if(S7::S7_inherits(record, class_record_fam)){
    "famg"
  } else if(S7::S7_inherits(record, class_record_sour)){
    "sour"
  } else if(S7::S7_inherits(record, class_record_repo)){
    "repo"
  } else if(S7::S7_inherits(record, class_record_media)){
    "media"
  } else if(S7::S7_inherits(record, class_record_note)){
    "note"
  } else {
    stop("Unrecognised record")
  }
  
}


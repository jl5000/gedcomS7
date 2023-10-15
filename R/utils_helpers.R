#' @include utils_at.R
NULL


#' Delete a structure from GEDCOM lines
#'
#' @param lines A character vector of GEDCOM lines.
#' @param line_no A line number where the structure is located.
#' @param containing_line Whether the line number is the first line of the structure or 
#' whether the line number references a line within the structure (but not more than one
#' level lower).
#'
#' @return The character vector of GEDCOM lines without the structure referenced by
#' the line_no. If the structure is an entire record, then any xref pointers to it
#' will also be replaced with a VOID pointer.
delete_ged_section <- function(lines, line_no, containing_line = TRUE){
  
  lvl <- parse_line_level(lines[line_no])
  if(!containing_line){ # move line_no to containing line
    while(line_no > 0 && parse_line_level(lines[line_no]) >= lvl){
      line_no <- line_no - 1
    }
    lvl <- parse_line_level(lines[line_no])
  }
  
  # Replace xref pointers with VOID
  section_xref <- parse_line_xref(lines[line_no])
  if(section_xref != ""){
    ptr_lines <- grepl(reg_xref(TRUE), parse_line_value(lines))
    lines[ptr_lines] <- sub(section_xref, "@VOID@", lines[ptr_lines])
  }
  
  # Delete section
  lines <- lines[-line_no]
  while(line_no <= length(lines) && 
        parse_line_level(lines[line_no]) > lvl){
    lines <- lines[-line_no]
  }
  lines
}

find_ged_values <- function(lines, 
                            tag,
                            return_list = FALSE){
  
  base_level <- parse_line_level(lines[1]) - 1
  
  # Ignore parent if lines describes a whole record
  if(parse_line_xref(lines[1]) != ""){
    lines <- lines[-1]
    base_level <- base_level + 1
  }
  
  if(length(tag) > length(lines)) return(character())
  
  for(level in seq_along(tag)){
    
    lines_lst <- split(lines, cumsum(parse_line_level(lines) == base_level + level)) |> 
      unname()
    
    lines_lst <- Filter(\(x) grepl(sprintf("^%s (%s)( (?s).*)?$", base_level + level, tag[level]), x[1], perl = TRUE), 
                        lines_lst)
    
    if(level == length(tag)){ # final tag
      if(return_list){
        return(lines_lst)
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
  vals <- parse_line_value(lines)
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
  
  cur_level <- parse_line_level(ged)
  remainder <- sub("^\\d+ ", "", ged)
  paste(cur_level + by, remainder)
}







remove_void_xrefs <- function(xrefs){
  xrefs[xrefs != "@VOID@"]
}

as.iterable <- function(x) {
  if(is.list(x) || is.atomic(x)) x else list(x)
}

add_census <- function(x, year, addr, xrefs_ages, roles, sour){
  
  year <- as.character(year)
  sour_xref <- x@next_xref["sour"]
  x <- push_record(x, sour)
  
  for(i in seq_along(xrefs_ages)){
    indi_rec <- pull_record(x, xrefs_ages[i])
    
    cens <- class_event_indi("CENS", date = year, address = addr)
    cens@age <- names(xrefs_ages)[i]
    
    cit <- class_citation(sour_xref = sour_xref,
                          fact_type = "CENS")
    
    if(roles[i] %in% val_roles()){
      cit@role <- roles[i]
      cit@role_phrase <- character()
    } else {
      cit@role <- "OTHER"
      cit@role_phrase <- roles[i]
    }
    
    cens@citations <- cit
    
    #is there already a census event with this year?
    existing <- overwrite <- FALSE
    for(i in seq_along(indi_rec@facts)){
      if(indi_rec@facts[[i]]@fact_type == "CENS" && indi_rec@facts[[i]]@date@as_val == year){
        existing <- TRUE
        message("Current:")
        message(paste(indi_rec@facts[[i]]@as_ged, collapse = "\n"))
        message("New:")
        message(paste(cens@as_ged, collapse = "\n"))
        overwrite <- utils::menu(c("Yes", "No"), title="Overwrite?")
        overwrite <- overwrite == 1L
        if(overwrite){
          indi_rec@facts[[i]] <- cens
        } else {
          break
        }
      }
    }
    
    if(!existing && !overwrite){
      indi_rec@facts <- append(indi_rec@facts, cens)
    }
    
    if(!existing || overwrite){
      x <- push_record(x, indi_rec)
    }
    
  }
  
  x
}

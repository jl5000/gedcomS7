
#' Delete a structure from GEDCOM lines
#'
#' @param lines A character vector of GEDCOM lines.
#' @param line_no A line number where the structure is located.
#' @param containing_line Whether the line number is the first line of the structure or 
#' whether the line number references a line within the structure (but not more than one
#' level lower).
#'
#' @returns The character vector of GEDCOM lines without the structure referenced by
#' the line_no.
#' @keywords internal
delete_ged_section <- function(lines, line_no, containing_line = TRUE){
  
  lvl <- parse_line_level(lines[line_no])
  if(lvl == 0 || (lvl == 1 && !containing_line))
    stop("If you want to remove a whole record you should use rm_records().")
  
  if(!containing_line){ # move line_no to containing line
    while(line_no > 0 && parse_line_level(lines[line_no]) >= lvl){
      line_no <- line_no - 1
    }
    lvl <- parse_line_level(lines[line_no])
  }
  
  # Delete section
  lines <- lines[-line_no]
  while(line_no <= length(lines) && 
        parse_line_level(lines[line_no]) > lvl){
    lines <- lines[-line_no]
  }
  lines
}

#' Delete potentially multiple structures from GEDCOM lines
#'
#' @param lines A character vector of GEDCOM lines.
#' @param line_fn A callback function which takes a single input, lines, and
#' returns an integer vector identifying the row of each structure to be deleted.
#' @param containing_line Whether the line number(s) returned by the callback
#' are the first line of the structure(s) or whether the line number(s) 
#' reference a line within the structure (but not more than one
#' level lower).
#'
#' @returns The character vector of GEDCOM lines without the structures 
#' identified by the callback function. 
#' @keywords internal
delete_ged_sections <- function(lines, line_fn, containing_line = TRUE){
  
  while(length(line_fn(lines)) > 0){
    line_no <- line_fn(lines)[1]
    lines <- delete_ged_section(lines, line_no, containing_line)
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
#' @returns A character vector of length one. It is either an empty string for a
#' zero length input, or takes the value of the first element.
#' @keywords internal
chronify <- function(x){
  if(length(x) == 0) return("")
  as.character(x)[1]
}

#' Increase the level of a vector of GEDCOM lines
#'
#' @param ged A character vector of GEDCOM lines.
#' @param by The number of levels to increment.
#'
#' @returns The vector of GEDCOM lines with incremented levels.
#' @keywords internal
increase_level <- function(ged, by = 1){
  if(length(ged) == 0) return(character())
  
  cur_level <- parse_line_level(ged)
  remainder <- sub("^\\d+ ", "", ged)
  paste(cur_level + by, remainder)
}

remove_void_xrefs <- function(xrefs){
  xrefs[xrefs != "@VOID@"]
}


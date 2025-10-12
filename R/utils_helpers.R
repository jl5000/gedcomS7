
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
    
    current_level <- base_level + level
    lines_lst <- split(lines, cumsum(parse_line_level(lines) == current_level)) |> 
      unname()
    
    # \\b needed to distinguish "1 NOTE" from "1 NO CHR"
    # Space cannot be used as some lines have no payload
    pattern <- sprintf("^%s (%s)\\b", current_level, tag[level])
    lines_lst <- Filter(\(x) grepl(pattern, x[1]), lines_lst)
    
    if(level == length(tag)){ # final tag
      if(return_list) return(lines_lst)
      
      # strip out subordinates
      lines_lst <- lapply(lines_lst, `[`, 1)
 
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
level_up <- function(ged, by = 1){
  if(length(ged) == 0) return(character())
  
  cur_level <- parse_line_level(ged)
  remainder <- sub("^\\d+ ", "", ged)
  paste(cur_level + by, remainder)
}

remove_void_xrefs <- function(xrefs){
  xrefs[xrefs != void_xref()]
}

to_console <- function(label, val, exdent){
  if(length(val) == 0 || isTRUE(val == "")) val <- "<Undefined>"
  cat(strwrap(val, 
              initial = sprintf(paste0("%-", exdent, "s"), label), 
              prefix = "", 
              exdent = exdent), 
      fill = TRUE)
}

to_console_value_with_phrase <- function(label, 
                                         value, 
                                         phrase, 
                                         exdent, 
                                         pattern = "%s (%s)"){
  if(length(phrase) == 1)
    value <- sprintf(pattern, value, phrase)
  
  to_console(label, value, exdent)
}

to_console_list <- function(label, values, exdent, prop = NULL){
  for(i in seq_along(values)){
    if(i == 1) intro <- label else intro <- ""
    if(is.null(prop)){
      to_console(intro, values[i], exdent)
    } else {
      to_console(intro, S7::prop(values[[i]], prop), exdent)
    }
  }
}

zipped_gedname <- function(filepath, must_exist = FALSE){
  
  if(tools::file_ext(filepath) != "zip") stop("File is not a zip archive")
    
  contents <- unzip(filepath, list = TRUE)
  ged_files <- contents[contents$Name == basename(contents$Name) &
                          tools::file_ext(contents$Name) == "ged",]
  
  if(nrow(ged_files) == 0){
    if(must_exist) stop("No GEDCOM files found in root of zip file")
    return("gedcom.ged")
  } else if(nrow(ged_files) == 1){
    return(ged_files$Name)
  } else {
    stop("More than one GEDCOM file found in root of zip file")
  }
  
}

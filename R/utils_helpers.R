#' @include utils_at.R
NULL

extract_ged_level <- function(lines){
  sub(reg_ged_line(), "\\1", lines) |> as.integer()
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
  
  lvl <- extract_ged_level(lines[line_no])
  if(!containing_line){ # move line_no to containing line
    while(line_no > 0 && extract_ged_level(lines[line_no]) >= lvl){
      line_no <- line_no - 1
    }
    lvl <- extract_ged_level(lines[line_no])
  }
  
  # Replace xref pointers with VOID
  section_xref <- extract_ged_xref(lines[line_no])
  if(section_xref != ""){
    ptr_lines <- grepl(reg_xref(TRUE), extract_ged_value(lines))
    lines[ptr_lines] <- sub(section_xref, "@VOID@", lines[ptr_lines])
  }
  
  # Delete section
  lines <- lines[-line_no]
  while(line_no <= length(lines) && 
        extract_ged_level(lines[line_no]) > lvl){
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
    
    lines_lst <- split(lines, cumsum(extract_ged_level(lines) == base_level + level)) |> 
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
  remainder <- sub("^\\d+ ", "", ged)
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

extract_vals_and_types <- function(lines, val_tag){
  val_lst <- find_ged_values(lines, val_tag, return_list = TRUE)
  if(length(val_lst) == 0) return(character())
  
  vals <- vapply(val_lst, \(x) extract_ged_value(x[1]), FUN.VALUE = character(1))
  types <- vapply(val_lst, \(x) {
    if(length(x) == 1) return("")
    extract_ged_value(x[2])
  }, FUN.VALUE = character(1))
  names(vals) <- types
  vals
}

obj_to_val <- function(obj){
  if("S7_object" %in% class(obj)){
    val <- obj@as_val
  } else {
    val <- obj # character/NULL
  }
  val
}



get_record_type <- function(record){
  
  if(S7::S7_inherits(record, class_record_indi)){
    "indi"
  } else if(S7::S7_inherits(record, class_record_fam)){
    "fam"
  } else if(S7::S7_inherits(record, class_record_sour)){
    "sour"
  } else if(S7::S7_inherits(record, class_record_repo)){
    "repo"
  } else if(S7::S7_inherits(record, class_record_media)){
    "media"
  } else if(S7::S7_inherits(record, class_record_note)){
    "note"
  } else if(S7::S7_inherits(record, class_record_subm)){
    "subm"
  } else {
    stop("Unrecognised record")
  }
  
}

#' Convert a GEDCOM date into a date object
#'
#' @param date_string A Gregorian date string.
#' @param minimise Whether to fill in missing date pieces so that the date is minimised. 
#' For example, if no month is given, January is used. If minimise = FALSE, December will be used.
#'
#' @return A date.
#' @export
#' @tests
#' expect_equal(parse_gedcom_date("2005"), as.Date("2005-01-01"))
#' expect_equal(parse_gedcom_date("2005", FALSE), as.Date("2005-12-31"))
#' expect_equal(parse_gedcom_date("JUL 1989"), as.Date("1989-07-01"))
#' expect_equal(parse_gedcom_date("JUL 1989", FALSE), as.Date("1989-07-31"))
#' expect_equal(parse_gedcom_date("25 MAR 1980"), as.Date("1980-03-25"))
#' expect_equal(parse_gedcom_date("25 MAR 1980", FALSE), as.Date("1980-03-25"))
parse_gedcom_date <- function(date_string, minimise = TRUE){
  
  ged_year <- sub(".* ", "", date_string)
  
  if(grepl("[A-Z]{3}", date_string)) {
    ged_month <- which(toupper(month.abb) == gsub("[^A-Z]", "", date_string))
  } else {
    if(minimise) ged_month <- 1 else ged_month <- 12
  }
  
  if(grepl("^\\d{1,2} ", date_string)) {
    ged_day <- gsub(" .*", "", date_string)
  } else {
    if(minimise) {
      ged_day <- 1
    } else {
      ged_day <- paste(10, ged_month, ged_year) |> 
        as.Date(format = "%d %m %Y") |> 
        days_in_month()
    }
  }
  
  paste(ged_day, ged_month, ged_year) |> 
    as.Date(format = "%d %m %Y")
}

days_in_month <- function(date) {
  m <- format(date, format="%m")
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  as.integer(format(date - 1, format="%d"))
}

#' Convert a GEDCOM age at event into decimalised years
#'
#' @param age_string A string describing an age at an event, 
#' e.g. "14y 3m 1w 2d".
#'
#' @return A numeric value giving the age in years.
#' @export
#' @tests
#' expect_equal(parse_gedcom_age("16y"), 16)
#' expect_equal(parse_gedcom_age("16y 6m"), 16.5)
#' expect_equal(parse_gedcom_age("73d"), 73/365)
#' expect_equal(parse_gedcom_age("12w"), 12/52)
#' expect_equal(parse_gedcom_age("3y 2m 1w 5d"), 3+(2/12)+(1/52)+(5/365))
parse_gedcom_age <- function(age_string) {

  years <- sub(".*?(\\d{1,3})y.*", "\\1", age_string)
  months <- sub(".*?(\\d{1,2})m.*", "\\1", age_string)
  weeks <- sub(".*?(\\d{1,2})w.*", "\\1", age_string)
  days <- sub(".*?(\\d{1,3})d.*", "\\1", age_string)
  
  if(age_string == years) years_num <- 0 else years_num <- as.numeric(years)
  if(age_string == months) months_prop <- 0 else months_prop <- as.numeric(months)/12
  if(age_string == weeks) weeks_prop <- 0 else weeks_prop <- as.numeric(weeks)/52
  if(age_string == days) days_prop <- 0 else days_prop <- as.numeric(days)/365
  
  years_num + months_prop + weeks_prop + days_prop
  
}
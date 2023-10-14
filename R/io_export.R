
#' Save a gedcom object to disk as a GEDCOM file
#' 
#' @details This function prepares the gedcom object and then writes it to the filepath.
#' Steps taken include filtering sensitive data, escaping "@" signs (with another "@") ,
#' and splitting long lines onto separate lines.
#'
#' @param gedcom A gedcom object.
#' @param filepath The full filepath to write to.
#' @param inc_confid Whether to include records that are marked as confidential.
#' @param inc_private Whether to include records that are marked as private.
#'
#' @return The filepath (invisibly).
#' @export
write_gedcom <- function(gedcom, 
                         filepath = file.choose(),
                         inc_confid = TRUE,
                         inc_private = TRUE,
                         inc_living = TRUE) {
  
  if(tolower(substr(filepath, nchar(filepath)-3 , nchar(filepath))) != ".ged")
    stop("Output is not being saved as a GEDCOM file (*.ged)")
  
  if(file.exists(filepath)) file.remove(filepath)
  
  # Write Byte Order Mark
  con <- file(filepath, encoding = "UTF-8", open = "wb")
  suppressWarnings(writeBin(as.raw(c(0xef, 0xbb, 0xbf)), con))
  close(con)
  
  con <- file(filepath, encoding = "UTF-8", open = "a")
  on.exit(close(con))
  
  lines <- gedcom@as_ged
  
  if(!inc_confid) lines <- remove_sensitive_sections(lines, "CONFIDENTIAL")
  if(!inc_private) lines <- remove_sensitive_sections(lines, "PRIVACY")
  
  # Moved to as_ged property
  #lines2 <- prepare_gedcom_lines(lines, inc_confid, inc_private)
  
  writeLines(lines, con)
  
  invisible(filepath)
}


#' Remove gedcom structures marked as sensitive
#'
#' @param lines A character vector of gedcom lines.
#' @param restriction Whether to remove structures marked as "CONFIDENTIAL" or "PRIVACY".
#'
#' @return A vector of sanitised GEDCOM lines.
remove_sensitive_sections <- function(lines, restriction){
  
  restriction_rows <- function(lines, restriction){
    intersect(
      grep("RESN", extract_ged_tag(lines)), 
      grep(restriction, extract_ged_value(lines))
    )
  }
  
  while(length(restriction_rows(lines, restriction)) > 0){
    
    line_no <- restriction_rows(lines, restriction)[1]
    
    lines <- delete_ged_section(lines, line_no, containing_line = FALSE)
    
  }
  lines
}

#' Prepare GEDCOM lines for export
#'
#' @param lines A character vector of gedcom lines.
#'
#' @return A vector of GEDCOM lines ready for export.
prepare_gedcom_lines <- function(lines){
  
  check_for_xref_mentions(lines)
    
  split_gedcom_values(lines) |> 
    add_at_escapes()
}

#' Check gedcom lines for inappropriate mentions of xrefs
#'
#' @param lines A character vector of gedcom lines.
#'
#' @return Nothing. If applicable, the function will result in a warning if
#' inappropriate mentions of xrefs are found.
check_for_xref_mentions <- function(lines){
  
  # Check for xrefs mentioned beyond pointers
  vals <- extract_ged_value(lines)
  line_no <- intersect(
    grep(reg_xref(FALSE), vals), # xrefs appear
    grep(reg_xref(TRUE), vals, invert = TRUE) # but they do not appear alone
  )
  
  if(length(line_no) > 0){
    warning("The following line numbers mention cross-reference identifiers - it is not recommended you refer to these directly as they can change between systems: ", 
            toString(line_no))
  }
}


add_at_escapes <- function(lines){
  
  # For lines with these patterns, we don't want to replace @ with @@
  exclude <- unique(c(
    grep(".+", extract_ged_xref(lines)), # start of record
    grep(reg_xref(TRUE), extract_ged_value(lines)) # xref pointer
  ))
  
  # sub() will pick up the first instance which should be the beginning of the value
  lines[-exclude] <- sub(" @", " @@", lines[-exclude])
  
  lines
  
}

#' Create continuation lines
#' 
#' This function introduces CONT lines for line values that contain line breaks.
#'
#' @param lines A character vector of gedcom lines.
#' 
#' @return A new character vector of gedcom lines, possibly expanded to include CONT lines.
#' @tests
#' test1 <- c(
#' "0 TEMP",
#' "1 TAG This is a line\nthen this\nand this\nalso this"
#' )
#' test2 <- c(
#'   "0 TEMP",
#'   "1 TAG A tag",
#'   "2 QUAY This is a line\nthen this\nand this\nalso this",
#'   "1 DATE Today"
#' )
#' 
#' expect_snapshot_value(split_gedcom_values(test1), "json2")
#' expect_snapshot_value(split_gedcom_values(test2), "json2")
split_gedcom_values <- function(lines) {

  lines <- gsub("\n\r|\r\n", "\n", lines)
  lines <- gsub("\r", "\n", lines)
  lines <- gsub("\n", "\n0 CONT ", lines)
  lines <- unlist(strsplit(lines, "\n"))
  
  lvl <- 0
  for(i in seq_along(lines)){
    if(substr(lines[i], 1, 6) == "0 CONT"){
      lines[i] <- increase_level(lines[i], lvl + 1)
    } else {
      lvl <- extract_ged_level(lines[i])
    }
  }
  
  lines
}


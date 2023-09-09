
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
                         inc_private = TRUE) {
  
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
  
  lines2 <- prepare_gedcom_lines(lines, inc_confid, inc_private)
  
  writeLines(lines2, con)
  
  invisible(filepath)
}

prepare_gedcom_lines <- function(lines, inc_confid, inc_private){
  
  #TODO: Need to remove records/facts and replace record pointers with @VOID@
  if(!inc_confid)
    lines <- lines
  
  if(!inc_private)
    lines <- lines
  
  # For lines with these patterns, we don't want to replace @ with @@
  exclude <- which(extract_ged_xref(lines) != "" | grepl(reg_xref(TRUE), extract_ged_value(lines)))
  lines[-exclude] <- gsub("@", "@@", lines[-exclude])
  
  lines <- split_gedcom_values(lines)
  
  lines
}

#' Convert the GEDCOM form to GEDCOM grammar
#' 
#' This function introduces CONT lines for line values that contain line breaks.
#'
#' @param lines A character vector of gedcom lines.
#' 
#' @return A new character vector of gedcom lines, possibly expanded to include CONT lines.
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


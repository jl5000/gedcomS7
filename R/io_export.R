
#' Save a gedcom object to disk as a GEDCOM file
#' 
#' @details This function prepares the gedcom object and then writes it to the filepath.
#' Steps taken include escaping "@" signs (with another "@") and splitting long lines onto
#' separate lines.
#'
#' @param gedcom A gedcom object.
#' @param filepath The full filepath to write to.
#'
#' @return The filepath (invisibly).
#' @export
write_gedcom <- function(gedcom, 
                         filepath = file.choose()) {
  
  if(tolower(substr(filepath, nchar(filepath)-3 , nchar(filepath))) != ".ged")
    stop("Output is not being saved as a GEDCOM file (*.ged)")
  
  if(file.exists(filepath)) file.remove(filepath)
  
  con <- file(filepath, encoding = "UTF-8", open = "wb")
  suppressWarnings(writeBin(as.raw(c(0xef, 0xbb, 0xbf)), con))
  close(con)
  
  con <- file(filepath, encoding = "UTF-8", open = "a")
  on.exit(close(con))
  
  gedcom@file_name <- basename(filepath)
  
  lines <- gedcom@as_ged
  
  # For lines with these patterns, we don't want to replace @ with @@
  exclude <- which(extract_ged_xref(lines) != "" | grepl(reg_xref(TRUE), extract_ged_value(lines)))
  lines[-exclude] <- gsub("@", "@@", lines[-exclude])
  
  lines <- split_gedcom_values(lines, char_limit = .pkgenv$gedcom_phys_value_limit)
  writeLines(lines, con)
  
  invisible(filepath)
}



#' Convert the GEDCOM form to GEDCOM grammar
#' 
#' This function introduces CONC/CONT lines for line values that exceed the given number of characters and
#' for lines containing line breaks.
#'
#' @param lines A character vector of gedcom lines.
#' @param char_limit Maximum string length of values.
#' 
#' @return A new character vector of gedcom lines, possibly expanded to include CONC/CONT lines.
split_gedcom_values <- function(lines, char_limit) {

  header <- lines[1:6] #header shouldn't contain CONT/CONC lines
  
  lines <- lines[-(1:6)]
  lines <- create_cont_lines(lines)
  lines <- create_conc_lines(lines, char_limit)
  
  c(header, lines)
}


#' Create CONTinuation lines
#'
#' @param lines A character vector of gedcom lines.
#'
#' @return A new character vector of gedcom lines, possibly expanded to include CONT lines.
create_cont_lines <- function(lines) {
 
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

#' Create CONCatenation lines
#'
#' @param lines A character vector of gedcom lines.
#' @param char_limit Character limit of line values.
#'
#' @return A new character vector of gedcom lines, possibly expanded to include CONC lines.
create_conc_lines <- function(lines, char_limit) {

  i <- 1
  while(i <= length(lines)){
    line_val <- extract_ged_value(lines[i])
    if(line_val != ""){ # there is a line value
      if(nchar(line_val) > char_limit){ # the line value is too long
        line_lvl <- extract_ged_level(lines[i])
        line_tag <- extract_ged_tag(lines[i])
        line_val_split <- strsplit(line_val, 
                                   sprintf("(?<=.{%s})", char_limit), 
                                   perl = TRUE)[[1]]
        new_lines <- paste(line_lvl + (line_tag != "CONT"), "CONC", line_val_split)
        new_lines[1] <- paste(line_lvl, line_tag, line_val_split[1])
        if(i > 1) pre <- lines[1:(i-1)] else pre <- NULL
        if(i < length(lines)) post <- lines[(i+1):length(lines)] else post <- NULL
        lines <- c(
          pre,
          new_lines,
          post
        )
        i <- i + length(line_val_split) - 1
      }
    }
    i <- i + 1
  }
  
  lines

}




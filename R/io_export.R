
#' Save a tidyged object to disk as a GEDCOM file
#' 
#' @details This function prepares the tidyged object and then writes it to the filepath.
#' Steps taken include escaping "@" signs (with another "@") and splitting long lines onto
#' separate lines.
#'
#' @param gedcom A tidyged object.
#' @param filepath The full filepath to write to.
#'
#' @return Nothing.
#' @export
#' @tests
#' expect_error(write_gedcom(read_gedcom(system.file("extdata", "555SAMPLE.GED", package = "tidyged.io")), 
#'                             "my_family.txt"))
#'  file.remove("my_family.txt")
#' expect_identical(
#'   read_gedcom(system.file("extdata", "555SAMPLE.GED", package = "tidyged.io")),
#'   read_gedcom(system.file("extdata", "555SAMPLE.GED", package = "tidyged.io")) |> 
#'     write_gedcom("555Sample.ged") |> 
#'     read_gedcom()
#' )
#' file.remove("555Sample.ged")
write_gedcom <- function(x, 
                         filepath = file.choose()) {
  
  if(tolower(substr(filepath, nchar(filepath)-3 , nchar(filepath))) != ".ged")
    stop("Output is not being saved as a GEDCOM file (*.ged)")
  
  if(file.exists(filepath)) file.remove(filepath)
  
  con <- file(filepath, encoding = "UTF-8", open = "wb")
  suppressWarnings(writeBin(as.raw(c(0xef, 0xbb, 0xbf)), con))
  close(con)
  
  con <- file(filepath, encoding = "UTF-8", open = "a")
  on.exit(close(con))
  
  x@file_name <- basename(filepath)
  
  ged <- x@as_ged
  
  # For lines with these patterns, we don't want to replace @ with @@
  exclude <- which(extract_ged_xref(ged) != "" | grepl(reg_xref(TRUE), extract_ged_value(ged)))
  ged[-exclude] <- gsub("@", "@@", ged[-exclude])
  
  ged <- split_gedcom_values(ged, char_limit = .pkgenv$gedcom_phys_value_limit)
  writeLines(ged, con)
  
  invisible(filepath)
}



#' Convert the GEDCOM form to GEDCOM grammar
#' 
#' This function introduces CONC/CONT lines for line values that exceed the given number of characters and
#' for lines containing line breaks.
#'
#' @param gedcom A tidyged object.
#' @param char_limit Maximum string length of values.
#' 
#' @return A tidyged object in the GEDCOM grammar ready to export.
split_gedcom_values <- function(ged, char_limit) {

  header <- ged[1:6] #header shouldn't contain CONT/CONC lines
  
  ged <- ged[-(1:6)]
  ged <- create_cont_lines(ged)
  ged <- create_conc_lines(ged, char_limit)
  
  c(header, ged)
}


#' Create CONTinuation lines
#'
#' @param lines Lines of a tidyged object.
#'
#' @return The same lines of the tidyged object, potentially with additional continuation lines.
create_cont_lines <- function(ged) {
 
  ged <- gsub("\n\r|\r\n", "\n", ged)
  ged <- gsub("\r", "\n", ged)
  ged <- gsub("\n", "\n0 CONT ", ged)
  ged <- unlist(strsplit(ged, "\n"))
  
  lvl <- 0
  for(i in seq_along(ged)){
    if(substr(ged[i], 1, 6) == "0 CONT"){
      ged[i] <- increase_level(ged[i], lvl + 1)
    } else {
      lvl <- extract_ged_level(ged[i])
    }
  }
  
  ged
  
}

#' Create CONCatenation lines
#'
#' @param lines Lines of a tidyged object.
#' @param char_limit Character limit of line values.
#'
#' @return The same lines of the tidyged object, potentially with additional concatenation lines.
create_conc_lines <- function(ged, char_limit) {

  i <- 1
  while(i <= length(ged)){
    line_val <- extract_ged_value(ged[i])
    if(line_val != ged[i]){ # there is a line value
      if(nchar(line_val) > char_limit){ # the line value is too long
        line_lvl <- extract_ged_level(ged[i])
        line_tag <- extract_ged_tag(ged[i])
        line_val_split <- strsplit(line_val, 
                                   sprintf("(?<=.{%s})", char_limit), 
                                   perl = TRUE)[[1]]
        new_lines <- paste(line_lvl + (line_tag != "CONT"), "CONC", line_val_split)
        new_lines[1] <- paste(line_lvl, line_tag, line_val_split[1])
        ged <- c(
          ged[1:(i-1)],
          new_lines,
          ged[(i+1):length(ged)]
        )
        i <- i + length(line_val_split) - 1
      }
    }
    i <- i + 1
  }
  
  ged

}





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
  xdf <- x@as_df
  
  xdf[!grepl(reg_xref(TRUE), value), value:= gsub("@", "@@", value)]
  
  xdf <- split_gedcom_values(xdf, char_limit = .pkgenv$gedcom_phys_value_limit)
  xdf[record == data.table::shift(record), record:= ""]
  xdf[record %in% c("HD","TR"), record:= ""] #First/last line
  
  xdf[, paste(level, record, tag, value)] |>
    sub(pattern = "(^\\d)  ", replacement = "\\1 ") |>
    sub(pattern = "(^\\d (@.+@)? ?\\w{3,5}) $", replacement = "\\1") |>
    writeLines(con)

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
split_gedcom_values <- function(xdf, char_limit) {

  header <- xdf[1:6] #header shouldn't contain CONT/CONC lines
  
  xdf <- xdf[-(1:6)]
  xdf <- create_cont_lines(xdf)
  xdf <- create_conc_lines(xdf, char_limit)
  
  rbind(header, xdf)
}


#' Create CONTinuation lines
#'
#' @param lines Lines of a tidyged object.
#'
#' @return The same lines of the tidyged object, potentially with additional continuation lines.
create_cont_lines <- function(xdf) {
  
  empty_vals <- "<>emptyvals<>"

  xdf[,value:= gsub("\n\r|\r\n", "\n", value)]
  xdf[,value:= gsub("\r", "\n", value)]
  xdf[, `:=`(split = grepl("\n", value), row = .I)]
  xdf[value == "", value:= empty_vals]
  xdf <- xdf[, lapply(.SD, function(x) unlist(data.table::tstrsplit(x, "\n"))),
     .SDcols = "value", by = c("record","level","tag","split","row")]
  xdf[value == empty_vals, value:= ""]
  xdf[split & 
        data.table::shift(split) & 
        row == data.table::shift(row), tag:= "CONT"]
  xdf[tag == "CONT", level:= level + 1]
  xdf[,`:=`(split = NULL, row = NULL)]
  xdf[]
}

#' Create CONCatenation lines
#'
#' @param lines Lines of a tidyged object.
#' @param char_limit Character limit of line values.
#'
#' @return The same lines of the tidyged object, potentially with additional concatenation lines.
create_conc_lines <- function(xdf, char_limit) {
  
  # A suitably unique string
  unique_delim <- "<>delimiter<>"
  empty_vals <- "<>emptyvals<>"
  
  xdf[,`:=`(split = nchar(value) > char_limit, row = .I)]
  xdf[,value:= gsub(paste0("(.{", char_limit, "})"), #add delimiters where
                    paste0("\\1", unique_delim), #the splits should occur
                    value)]
  xdf[,value:= gsub(paste0(unique_delim, "$"), "", value)] #remove last delimiter
  xdf[value == "", value:= empty_vals]
  xdf <- xdf[, lapply(.SD, function(x) unlist(data.table::tstrsplit(x, unique_delim))),
             .SDcols = "value", by = c("record","level","tag","split","row")]
  xdf[value == empty_vals, value:= ""]
  xdf[split & row == data.table::shift(row) & tag != "CONT", `:=`(tag = "CONC", level = level + 1)]
  xdf[split & row == data.table::shift(row), tag:= "CONC"] # remaining CONC lines not stemming from CONT lines
  xdf[,`:=`(split = NULL, row = NULL)]
  xdf[]
}




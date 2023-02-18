
#' Import a GEDCOM file
#'
#' Imports a *.ged file and creates a gedcom object.
#'
#' @param filepath The full filepath of the GEDCOM file.
#'
#' @return A gedcom S7 object.
#' @export
read_gedcom <- function(filepath = file.choose()) {
  
  if(tolower(substr(filepath, nchar(filepath)-3 , nchar(filepath))) != ".ged")
    stop("GEDCOM file should have a .ged extension")
  
  con <- file(filepath, encoding = "UTF-8")
  on.exit(close(con))
  
  ged_lines <- readLines(con)
  
  validate_lines(ged_lines)
  
  ged_lines <- combine_gedcom_values(ged_lines) |>
    gsub(pattern = "@@", replacement = "@")
  
  records_lst <- split(ged_lines, cumsum(substr(ged_lines, 1, 1) == "0"))
  
  parse_records(records_lst)

}



validate_lines <- function(lines){
  
  invalid_lines <- grep(reg_ged_line(), lines, invert = TRUE)
  
  if(length(invalid_lines) > 0)
    stop(paste(c("The following lines are invalid:", 
                 paste(invalid_lines, lines[invalid_lines], sep = ": ")), 
               collapse = "\n"))
  
  NULL
}



#' Convert the GEDCOM grammar to the GEDCOM form
#' 
#' This function applies concatenation indicated by CONC/CONT lines.
#'
#' @param lines A character vector of gedcom lines.
#'
#' @return A new character vector of gedcom lines.
combine_gedcom_values <- function(lines) {
  
  reg_cont_conc <- sprintf("^[1-6] (CONT|CONC) (.*)$")
  cont_conc_lines <- grep(reg_cont_conc, lines)
  if(length(cont_conc_lines) == 0) return(lines)
  
  unique_delim <- "<><>unique_delim<><>"
  
  lines <- lines |> 
    gsub(pattern = "\n\r|\r\n", replacement = "\n") |> 
    gsub(pattern = "\r", replacement = "\n") |>
    sub(pattern = "^([1-6] CONT )", replacement = "\\1\n")
  
  # Prepare lines for merging
  lines[cont_conc_lines] <- sub(reg_cont_conc, "\\2",
                                lines[cont_conc_lines])
  lines[-cont_conc_lines] <- paste0(unique_delim,
                                    lines[-cont_conc_lines])
  
  # Merge and seperate again
  lines <- paste(lines, collapse = "") |>
    strsplit(unique_delim) |>
    unlist()
  
  lines[-1] # delete empty line introduced
}


parse_records <- function(records_lst){
  
  if(!grepl(sprintf("^0 %s SUBM$", reg_xref(FALSE)), records_lst[[2]][1]))
    stop("The record immediately after the header record must be the Submitter record")
  
  if(records_lst[[length(records_lst)]][1] != "0 TRLR")
    stop("The file does not end with a TRLR record")
  
  records_lst <- records_lst[-length(records_lst)]
  
  # parse subm and header
  x <- create_gedcom(records_lst)
  records_lst <- records_lst[-(1:2)]
  
  subset_recs <- function(rec_lst, pattern){
    recs <- Filter(\(x) grepl(sprintf(pattern, reg_xref(FALSE)), x[1]), rec_lst)
    names(recs) <- sapply(recs, \(x) regmatches(x[1], regexpr(reg_xref(FALSE), x[1])))
    recs
  }

  S7::props(x) <- list(
    indi = subset_recs(records_lst, "^0 %s INDI$"),
    famg = subset_recs(records_lst, "^0 %s FAM$"),
    sour = subset_recs(records_lst, "^0 %s SOUR$"),
    repo = subset_recs(records_lst, "^0 %s REPO$"),
    media = subset_recs(records_lst, "^0 %s OBJE$"),
    note = subset_recs(records_lst, "^0 %s NOTE .+$")
  )

  x
}


create_gedcom <- function(records_lst){
  
  subm_lines <- records_lst[[2]]

  subm_nts <- find_ged_values(subm_lines, "NOTE")
  
  subm <- class_subm(
    xref = extract_ged_xref(subm_lines[1]),
    name = find_ged_values(subm_lines, "NAME"),
    address = extract_address(subm_lines),
    media_links = find_ged_values(subm_lines, "OBJE"),
    auto_id = find_ged_values(subm_lines, "RIN"),
    note_links = subm_nts[grepl(reg_xref(TRUE), subm_nts)],
    notes = subm_nts[!grepl(reg_xref(TRUE), subm_nts)],
    last_updated = extract_change_date(subm_lines)
  )
  
  hd_lines <- records_lst[[1]]
  
  class_gedcomS7(
    system_id = find_ged_values(hd_lines, c("HEAD","SOUR")),
    product_name = find_ged_values(hd_lines, c("HEAD","SOUR","NAME")),
    product_version = find_ged_values(hd_lines, c("HEAD","SOUR","VERS")),
    business_name = find_ged_values(hd_lines, c("HEAD","SOUR","CORP")),
    business_address = extract_address(hd_lines, c("HEAD","SOUR","CORP")),
    source_data_name = find_ged_values(hd_lines, c("HEAD","SOUR","DATA")),
    source_data_pubdate = find_ged_values(hd_lines, c("HEAD","SOUR","DATA","DATE")) |> toupper(),
    source_data_copyright = find_ged_values(hd_lines, c("HEAD","SOUR","DATA","COPR")),
    receiving_system = find_ged_values(hd_lines, c("HEAD","DEST")),
    creation_date = find_ged_values(hd_lines, c("HEAD","DATE")) |> toupper(),
    creation_time = find_ged_values(hd_lines, c("HEAD","DATE","TIME")),
    language = find_ged_values(hd_lines, c("HEAD","LANG")),
    xref_subm = find_ged_values(hd_lines, c("HEAD","SUBM")),
    file_name = find_ged_values(hd_lines, c("HEAD","FILE")),
    gedcom_copyright = find_ged_values(hd_lines, c("HEAD","COPR")),
    content_description = find_ged_values(hd_lines, c("HEAD","NOTE")),
    subm = subm
  )
  
}


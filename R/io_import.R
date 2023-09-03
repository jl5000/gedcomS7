
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
  
  ged_lines <- remove_at_escapes(ged_lines) |> 
    combine_cont_lines()
  
  records_lst <- split(ged_lines, cumsum(substr(ged_lines, 1, 1) == "0"))
  
  parse_records(records_lst)

}


validate_lines <- function(lines){
  
  invalid_lines <- grep(reg_ged_line(), lines, invert = TRUE)
  
  if(length(invalid_lines) > 0)
    stop(paste(c("The following lines are invalid:", 
                 paste(invalid_lines, lines[invalid_lines], sep = ": ")), 
               collapse = "\n"))
  
  #check characters in components
  
  #non-greg calendars not allowed
  
  if(lines[1] != "0 HEAD")
    stop("The file does not start with a HEAD record")
  
  if(lines[length(lines)] != "0 TRLR")
    stop("The file does not end with a TRLR record")
  
  # UIDs should only appear once
  uid_rows <- grep("^1 UID ", lines)
  if(length(uid_rows) > 0){
    vals <- extract_ged_value(lines)
    uids <- vals[uid_rows]
    dupes <- duplicated(uids)
    if(sum(dupes) > 0)
      stop("Some UIDs are duplicated: ", toString(unique(uids[dupes])))
  }
  
  NULL
}

remove_at_escapes <- function(lines){
  
  vals <- extract_ged_value(lines)
  
  escape_lines <- which(substr(vals, 1, 2) == "@@")
  
  if(length(escape_lines) == 0) return(lines)
  
  lines[escape_lines] <- sub("^([0-9] [A-Z0-9_]+ )@", "\\1", lines[escape_lines])
  lines
}

#' Combine continuation lines into the parent line
#' 
#' @param lines A character vector of gedcom lines.
#'
#' @return A new character vector of gedcom lines.
combine_cont_lines <- function(lines) {
  
  reg_cont <- sprintf("^[1-9] CONT (.*)$")
  cont_lines <- grep(reg_cont, lines)
  if(length(cont_lines) == 0) return(lines)
  
  unique_delim <- "<><>unique_delim<><>"
  
  lines <- sub("^([1-9] CONT )", "\\1\n", lines)
  
  # Prepare lines for merging
  lines[cont_lines] <- sub(reg_cont, "\\1", lines[cont_lines])
  lines[-cont_lines] <- paste0(unique_delim, lines[-cont_lines])
  
  # Merge and separate again
  lines <- paste(lines, collapse = "") |>
    strsplit(unique_delim) |>
    unlist()
  
  lines[-1] # delete empty line introduced
}


parse_records <- function(records_lst){
  
  # Remove TRLR
  records_lst <- records_lst[-length(records_lst)]
  
  # parse header
  x <- create_gedcom(records_lst[[1]])
  records_lst <- records_lst[-1]
  
  subset_recs <- function(rec_lst, rec_type){
    recs <- Filter(\(x) grepl(sprintf("^0 %s %s", reg_xref(FALSE), rec_type), x[1]), rec_lst)
    names(recs) <- sapply(recs, \(rec) extract_ged_xref(rec[1]))
    recs
  }
  
  S7::props(x) <- list(
    indi = subset_recs(records_lst, "INDI"),
    fam = subset_recs(records_lst, "FAM"),
    sour = subset_recs(records_lst, "SOUR"),
    repo = subset_recs(records_lst, "REPO"),
    media = subset_recs(records_lst, "OBJE"),
    note = subset_recs(records_lst, "SNOTE"),
    subm = subset_recs(records_lst, "SUBM")
  )
  
  x
}


create_gedcom <- function(hd_lines){
  
  sour <- NULL
  product_id <- find_ged_values(hd_lines, c("HEAD","SOUR"))
  
  if(length(product_id) == 1){
    
    sour <- class_gedcom_source(
      product_id = find_ged_values(hd_lines, c("HEAD","SOUR")),
      product_name = find_ged_values(hd_lines, c("HEAD","SOUR","NAME")),
      product_version = find_ged_values(hd_lines, c("HEAD","SOUR","VERS")),
      business_name = find_ged_values(hd_lines, c("HEAD","SOUR","CORP")),
      business_address = extract_address(hd_lines, c("HEAD","SOUR","CORP")),
      phone_numbers = find_ged_values(hd_lines, c("HEAD","SOUR","CORP","PHON")),
      emails = find_ged_values(hd_lines, c("HEAD","SOUR","CORP","EMAIL")),
      faxes = find_ged_values(hd_lines, c("HEAD","SOUR","CORP","FAX")),
      web_pages = find_ged_values(hd_lines, c("HEAD","SOUR","CORP","WWW|URL")),
      data_name = find_ged_values(hd_lines, c("HEAD","SOUR","DATA")),
      data_pubdate = find_ged_values(hd_lines, c("HEAD","SOUR","DATA","DATE")) |> toupper(),
      data_pubtime = find_ged_values(hd_lines, c("HEAD","SOUR","DATA","TIME")),
      data_copyright = find_ged_values(hd_lines, c("HEAD","SOUR","DATA","COPR"))
    )
  }
  
  class_gedcomS7(
    gedcom_version = find_ged_values(hd_lines, c("HEAD","GEDC","VERS")),
    ext_tags = find_ged_values(hd_lines, c("HEAD","SCHMA","TAG")),
    source = sour,
    destination = find_ged_values(hd_lines, c("HEAD","DEST")),
    creation_date = find_ged_values(hd_lines, c("HEAD","DATE")) |> toupper(),
    creation_time = find_ged_values(hd_lines, c("HEAD","DATE","TIME")),
    subm_xref = find_ged_values(hd_lines, c("HEAD","SUBM")),
    gedcom_copyright = find_ged_values(hd_lines, c("HEAD","COPR")),
    default_language = find_ged_values(hd_lines, c("HEAD","LANG")),
    default_place_form = find_ged_values(hd_lines, c("HEAD","PLAC","FORM")),
    notes = extract_notes(hd_lines),
    note_xrefs = find_ged_values(hd_lines, c("HEAD","SNOTE"))
  )
  
}


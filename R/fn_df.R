
get_records <- function(x, xrefs, rec_type){
  rec_list <- S7::prop(x@records@RAW, rec_type)
  xrefs <- xrefs %||% names(rec_list)
  invalid <- setdiff(xrefs, names(rec_list))
  if(length(invalid) > 0){
    xrefs <- setdiff(xrefs, invalid)
    warning("The following xrefs are not of the right type: ", toString(invalid))
  }
  rec_list <- rec_list[xrefs]
}

mutate_generic_values <- function(df, lines){
  
  df$unique_ids <- find_ged_values(lines, "UID") |> 
    paste(collapse = ";")
  exids <- parse_vals_and_types(lines, "EXID")
  df$ext_ids <- paste(names(exids), exids, sep = "/", collapse = ";") |> 
    chronify()
  refns <- parse_vals_and_types(lines, "REFN")
  df$user_ids <- paste(names(refns), refns, sep = "=", collapse = ";") |> 
    chronify()
  df$locked <- any(grepl("^1 RESN .*LOCKED", lines))
  df$private <- any(grepl("^1 RESN .*PRIVACY", lines))
  df$confidential <- any(grepl("^1 RESN .*CONFIDENTIAL", lines))
  df$last_modified <- chronify(find_ged_values(lines, c("CHAN","DATE")))
  
  df
}

df_recs <- function(rec_list, extract_fn){
  prog_threshold <- 200
  
  pb <- NULL
  if(length(rec_list) > prog_threshold)
    pb <- utils::txtProgressBar(max = length(rec_list), style = 3)
  
  rows <- lapply(
    rec_list, \(lines){
      df <- extract_fn(lines) |> 
        mutate_generic_values(lines)
      
      if(!is.null(pb)) utils::setTxtProgressBar(pb, pb$getVal() + 1)
      
      as.data.frame(df)
    }
  )
  
  df <- do.call(rbind, rows)
  
  if(!is.null(pb)) close(pb)
  
  cbind(xref = rownames(df), data.frame(df, row.names=NULL))
}

#' Summarise records of a particular type in a dataframe
#'
#' @param x A gedcom object.
#' @param xrefs A vector of xrefs to summarise. If this is left NULL,
#' all relevant xrefs will be used.
#'
#' @returns A dataframe summarising a record on each row.
#' @export
#' @tests
#' indi_df <- df_indi(test_ged())
#' expect_equal(nrow(indi_df), 6)
#' expect_equal(indi_df$name[2], "Father /Bloggs/")
#' expect_equal(indi_df$sex[3], "F")
#' expect_equal(indi_df$birth_date[1], "20 MAR 1967")
#' expect_equal(indi_df$fam_as_child[5], "@F2@")
#' expect_equal(indi_df$fam_as_child[6], "@F2@")
df_indi <- function(x, xrefs = NULL){
  rec_list <- get_records(x, xrefs, "INDI")
  if(length(rec_list) == 0) return(NULL)
  
  extract_rec_values <- \(lines){
    df <- list()
    xref <- parse_line_xref(lines[1])
    df$name <- chronify(find_ged_values(lines, "NAME"))
    df$sex <- chronify(find_ged_values(lines, "SEX"))
    df$birth_date <- chronify(find_ged_values(lines, c("BIRT","DATE")))
    df$birth_place <- chronify(find_ged_values(lines, c("BIRT","PLAC")))
    df$is_alive <- is_alive(lines)
    df$death_date <- chronify(find_ged_values(lines, c("DEAT","DATE")))
    df$death_place <- chronify(find_ged_values(lines, c("DEAT","PLAC")))
    df$fam_as_child <- get_fam_as_child(x, xref, "BIRTH") |> 
      paste(collapse = ";")
    df$fam_as_spouse <- get_fam_as_spouse(x, xref) |> 
      paste(collapse = ";")
    df
  }
  
  df_recs(rec_list, extract_rec_values)
}

#' @rdname df_indi
#' @export
#' @tests
#' fam_df <- df_fam(test_ged())
#' expect_equal(nrow(fam_df), 3)
#' expect_equal(fam_df$husb_xref[1], "@I2@")
#' expect_equal(fam_df$wife_xref[2], "@I4@")
#' expect_equal(fam_df$unique_ids[3], "f511d543-43c2-4642-b7dd-31c1a2a6bbc2")
#' expect_equal(fam_df$user_ids[3], "My ID=1234")
#' expect_equal(fam_df$ext_ids[3], "http://www.website.com/page1")
df_fam <- function(x, xrefs = NULL){
  rec_list <- get_records(x, xrefs, "FAM")
  if(length(rec_list) == 0) return(NULL)
  
  extract_rec_values <- \(lines){
    df <- list()
    df$husb_xref <- chronify(find_ged_values(lines, "HUSB"))
    df$wife_xref <- chronify(find_ged_values(lines, "WIFE"))
    df$chil_xref <- find_ged_values(lines, "CHIL") |>
      paste(collapse = ";")
    df$marr_date <- chronify(find_ged_values(lines, c("MARR","DATE")))
    df$marr_place <- chronify(find_ged_values(lines, c("MARR","PLAC")))
    df
  }

  df_recs(rec_list, extract_rec_values)
}

#' @rdname df_indi
#' @export
df_sour <- function(x, xrefs = NULL){
  rec_list <- get_records(x, xrefs, "SOUR")
  if(length(rec_list) == 0) return(NULL)
  
  extract_rec_values <- \(lines){
    df <- list()
    df$originator <- chronify(find_ged_values(lines, "AUTH"))
    df$title <- chronify(find_ged_values(lines, "TITL"))
    df$repo_xref <- find_ged_values(lines, "REPO") |>
      paste(collapse = ";")
    df
  }
  
  df_recs(rec_list, extract_rec_values)
}

#' @rdname df_indi
#' @export
df_repo <- function(x, xrefs = NULL){
  rec_list <- get_records(x, xrefs, "REPO")
  if(length(rec_list) == 0) return(NULL)
  
  extract_rec_values <- \(lines){
    df <- list()
    df$name <- chronify(find_ged_values(lines, "NAME"))
    df$address <- chronify(find_ged_values(lines, "ADDR"))
    df
  }
  
  df_recs(rec_list, extract_rec_values)
}

#' @rdname df_indi
#' @export
df_media <- function(x, xrefs = NULL){
  rec_list <- get_records(x, xrefs, "OBJE")
  if(length(rec_list) == 0) return(NULL)
  
  extract_rec_values <- \(lines){
    df <- list()
    df$num_files <- length(find_ged_values(lines, "FILE"))
    df$paths <- find_ged_values(lines, "FILE") |>
      paste(collapse = ";")
    df
  }
  
  df_recs(rec_list, extract_rec_values)
}

#' @rdname df_indi
#' @export
df_note <- function(x, xrefs = NULL){
  rec_list <- get_records(x, xrefs, "SNOTE")
  if(length(rec_list) == 0) return(NULL)
  
  extract_rec_values <- \(lines){
    df <- list()
    txt <- parse_line_value(lines)
    if(nchar(txt) > 50) 
      txt <- paste0(substr(txt, 1, 47), "...")
    
    df$text <- txt
    df$language <- chronify(find_ged_values(lines, "LANG"))
    df
  }
  
  df_recs(rec_list, extract_rec_values)
}

#' @rdname df_indi
#' @export
df_subm <- function(x, xrefs = NULL){
  rec_list <- get_records(x, xrefs, "SUBM")
  if(length(rec_list) == 0) return(NULL)
  
  extract_rec_values <- \(lines){
    df <- list()
    df$name <- chronify(find_ged_values(lines, "NAME"))
    df$address <- chronify(find_ged_values(lines, "ADDR"))
    df
  }
  
  df_recs(rec_list, extract_rec_values)
}

#' Summarise an individual's attributes/events in a dataframe
#'
#' @param x A gedcom object.
#' @param xref The cross-reference identifier of an individual record.
#'
#' @returns A dataframe summarising an attribute/event on each row.
#' @export
#' @tests
#' fact_df <- df_indi_facts(test_ged(), "@I1@")
#' expect_equal(fact_df$type[1], "Birth")
#' expect_equal(fact_df$type[4], "Other attribute")
#' expect_equal(fact_df$val[2], "5 ft 10, brown hair, brown eyes")
#' expect_equal(fact_df$desc[4], "Disability")
#' expect_equal(fact_df$date[3], "8 APR 2018")
#' expect_equal(fact_df$place[1], "California, USA")
#' expect_equal(fact_df$age[3], "51y")
df_indi_facts <- function(x, xref){
  check_indi_rec(x, xref)
  
  indi <- suppressWarnings(pull_record(x, xref))
  fcts <- indi@facts
  
  df <- data.frame(
    xref = xref,
    type = vapply(fcts, \(fct) chronify(fct@fact_type), FUN.VALUE = character(1)),
    val = vapply(fcts, \(fct) chronify(fct@fact_val), FUN.VALUE = character(1)),
    desc = vapply(fcts, \(fct) chronify(fct@fact_desc), FUN.VALUE = character(1)),
    date = vapply(fcts, \(fct) chronify(fct@FACT_DATE), FUN.VALUE = character(1)),
    place = vapply(fcts, \(fct) chronify(fct@FACT_LOCATION), FUN.VALUE = character(1)),
    age = vapply(fcts, \(fct) chronify(fct@age), FUN.VALUE = character(1))
  )
  
  indi_facts <- c(val_individual_attribute_types(TRUE),
                  val_individual_event_types(TRUE))
  df$type <- names(indi_facts)[match(df$type, indi_facts)]
  df
}

#' Summarise a family's attributes/events in a dataframe
#'
#' @param x A gedcom object.
#' @param xref The cross-reference identifier of a family record.
#'
#' @returns A dataframe summarising an attribute/event on each row.
#' @export
#' @tests
#' fact_df <- df_fam_facts(test_ged(), "@F1@")
#' expect_equal(fact_df$type[1], "Marriage")
#' expect_equal(fact_df$date[1], "MAR 1965")
#' expect_equal(fact_df$place[1], "London, England")
#' expect_equal(fact_df$husb_age[1], "28y")
#' expect_equal(fact_df$wife_age[1], "25y")
df_fam_facts <- function(x, xref){
  check_fam_rec(x, xref)
  
  fam <- suppressWarnings(pull_record(x, xref))
  fcts <- fam@facts
  
  df <- data.frame(
    xref = xref,
    type = vapply(fcts, \(fct) chronify(fct@fact_type), FUN.VALUE = character(1)),
    val = vapply(fcts, \(fct) chronify(fct@fact_val), FUN.VALUE = character(1)),
    desc = vapply(fcts, \(fct) chronify(fct@fact_desc), FUN.VALUE = character(1)),
    date = vapply(fcts, \(fct) chronify(fct@FACT_DATE), FUN.VALUE = character(1)),
    place = vapply(fcts, \(fct) chronify(fct@FACT_LOCATION), FUN.VALUE = character(1)),
    husb_age = vapply(fcts, \(fct) chronify(fct@husb_age), FUN.VALUE = character(1)),
    wife_age = vapply(fcts, \(fct) chronify(fct@wife_age), FUN.VALUE = character(1))
  )
  
  fam_facts <- c(val_family_attribute_types(TRUE),
                  val_family_event_types(TRUE))
  df$type <- names(fam_facts)[match(df$type, fam_facts)]
  df
}

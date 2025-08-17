

#' Create a GEDCOM source object
#' 
#' @inheritParams prop_definitions 
#' @param product_id An identifier for the product producing this dataset.
#' @param product_name The name of the product producing this dataset.
#' @param product_version The version of the product producing this dataset.
#' @param business_name The name of the business, corporation, or person that produced 
#' or commissioned the product.
#' @param business_address The address of the business, corporation, or person that produced 
#' or commissioned the product. The address is given either as a `Address()` object or as a 
#' character string. This would be as written on a mailing label with new lines separated by \\n.
#' @param data_name Deprecated.
#' @param data_pubdate Deprecated.
#' @param data_pubtime Deprecated.
#' @param data_copyright Deprecated.
#' @returns An S7 object representing a GEDCOM HEAD.SOUR.
GedcomSource <- S7::new_class(
  "GedcomSource",
  parent = GedcomS7class,
  properties = list(
    product_id = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_size(value, 1, 1, 1)
                                  }),
    product_name = S7::new_property(S7::class_character,
                                    validator = function(value){
                                      chk_input_size(value, 0, 1, 1)
                                    }),
    product_version = S7::new_property(S7::class_character,
                                       validator = function(value){
                                         chk_input_size(value, 0, 1, 1)
                                       }),
    business_name = S7::new_property(S7::class_character,
                                     validator = function(value){
                                       chk_input_size(value, 0, 1, 1)
                                     }),
    business_address = S7::new_property(NULL | S7::new_S3_class("gedcomS7::Address"),
                                        getter = function(self) self@business_address,
                                        setter = function(self, value){
                                          self@business_address <- as.S7class(value, gedcomS7::Address)
                                          self
                                        }),
    phone_numbers = prop_anything(),
    emails = prop_anything(),
    faxes = prop_anything(),
    web_pages = prop_anything(),
    data_name = S7::new_property(S7::class_character,
                                 validator = function(value){
                                   chk_input_size(value, 0, 1, 1)
                                 }),
    data_pubdate = S7::new_property(S7::class_character | 
                                      S7::new_S3_class("gedcomS7::DateExact"),
                                    getter = function(self) self@data_pubdate,
                                    setter = function(self, value){
                                      if(is.character(value)) value <- toupper(value)
                                      self@data_pubdate <- value
                                      self
                                    },
                                    validator = function(value){
                                      c(
                                        chk_input_size(value, 0, 1),
                                        chk_input_pattern(value, reg_date_exact())
                                      )
                                    }),
    data_pubtime = S7::new_property(S7::class_character | 
                                      S7::new_S3_class("gedcomS7::Time"),
                                    validator = function(value){
                                      c(
                                        chk_input_size(value, 0, 1),
                                        chk_input_pattern(value, reg_time())
                                      )
                                    }),
    data_copyright = S7::new_property(S7::class_character,
                                      validator = function(value){
                                        chk_input_size(value, 0, 1, 1)
                                      }),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 SOUR %s", self@product_id),
          sprintf("1 VERS %s", self@product_version),
          sprintf("1 NAME %s", self@product_name),
          sprintf("1 CORP %s", self@business_name),
          obj_to_ged(self@business_address, "ADDR") |> increase_level(by = 2),
          sprintf("2 PHON %s", self@phone_numbers),
          sprintf("2 EMAIL %s", self@emails),
          sprintf("2 FAX %s", self@faxes),
          sprintf("2 WWW %s", self@web_pages),
          sprintf("1 DATA %s", self@data_name),
          sprintf("2 DATE %s", obj_to_val(self@data_pubdate)),
          sprintf("3 TIME %s", obj_to_val(self@data_pubtime)),
          sprintf("2 COPR %s", self@data_copyright)
        )
      }
    )),
  validator = function(self){
    c(
      chk_input_parents(self@product_name, "@product_name", self@product_id, "@product_id"),
      chk_input_parents(self@product_version, "@product_version", self@product_id, "@product_id"),
      chk_input_parents(self@business_name, "@business_name", self@product_id, "@product_id"),
      chk_input_parents(self@business_address, "@business_address", self@business_name, "@business_name"),
      chk_input_parents(self@phone_numbers, "@phone_numbers", self@business_name, "@business_name"),
      chk_input_parents(self@emails, "@emails", self@business_name, "@business_name"),
      chk_input_parents(self@faxes, "@faxes", self@business_name, "@business_name"),
      chk_input_parents(self@web_pages, "@web_pages", self@business_name, "@business_name"),
      chk_input_parents(self@data_name, "@data_name", self@product_id, "@product_id"),
      chk_input_parents(self@data_pubdate, "@data_pubdate", self@data_name, "@data_name"),
      chk_input_parents(self@data_pubtime, "@data_pubtime", self@data_pubdate, "@data_pubdate"),
      chk_input_parents(self@data_copyright, "@data_copyright", self@data_name, "@data_name")
    )
  }
)



#' Create a GEDCOM header object
#' 
#' @inheritParams prop_definitions 
#' @param gedcom_version The version number of the official specification that this 
#' GEDCOM conforms to. This must include the major and minor version (for example, “7.0”); 
#' it may include the patch as well (for example, “7.0.1”), but doing so is not required. 
#' @param ext_tags Not supported.
#' @param source A `GedcomSource()` object describing the software that has generated the GEDCOM.`
#' @param destination An identifier for the system expected to receive this GEDCOM.
#' @param creation_date The creation date of the file given either as a formatted GEDCOM string, or a
#' `DateExact()` object.
#' @param creation_time The creation time of the file given either as a formatted GEDCOM string, or a
#' `Time()` object.
#' @param subm_xref The cross-reference identifier of the primary submitter.
#' @param gedcom_copyright A copyright statement, as appropriate for the copyright laws applicable to 
#' this data.
#' @param default_language The default language for the entire GEDCOM object.
#' @param default_place_form The default form for place names in the GEDCOM object. 
#' A comma-separated string of jurisdictional titles. 
#' For example "City, County, State, Country".
#' 
#' @returns An S7 object representing a GEDCOM header.
GedcomHeader <- S7::new_class(
  "GedcomHeader",
  parent = GedcomS7class,
  properties = list(
    gedcom_version = S7::new_property(S7::class_character,
                                      validator = function(value){
                                        c(
                                          chk_input_size(value, 1, 1),
                                          chk_input_pattern(value, "^\\d+\\.\\d+(\\.\\d+)?$")
                                        )
                                      }),
    ext_tags = S7::new_property(S7::class_character,
                                validator = function(value){
                                  #chk_input_size(value, 0, 0), # extension tags not supported
                                }),
    source = S7::new_property(NULL | S7::new_S3_class("gedcomS7::GedcomSource"),
                              validator = function(value){
                                chk_input_size(value, 0, 1)
                              }),
    destination = S7::new_property(S7::class_character,
                                   validator = function(value){
                                     chk_input_size(value, 0, 1, 1)
                                   }),
    creation_date = S7::new_property(S7::class_character | 
                                       S7::new_S3_class("gedcomS7::DateExact"),
                                     getter = function(self) self@creation_date,
                                     setter = function(self, value){
                                       if(length(value) == 0) value <- date_exact_current()
                                       if(is.character(value)) value <- toupper(value)
                                       self@creation_date <- value
                                       self
                                     },
                                     validator = function(value){
                                       c(
                                         chk_input_size(value, 0, 1),
                                         chk_input_pattern(value, reg_date_exact())
                                       )
                                     }),
    creation_time = S7::new_property(S7::class_character | 
                                       S7::new_S3_class("gedcomS7::Time"),
                                     validator = function(value){
                                       c(
                                         chk_input_size(value, 0, 1),
                                         chk_input_pattern(value, reg_time())
                                       )
                                     }),
    subm_xref = prop_xref(NULL, 0, 1),
    gedcom_copyright = S7::new_property(S7::class_character,
                                        validator = function(value){
                                          chk_input_size(value, 0, 1, 1)
                                        }),
    default_language = S7::new_property(S7::class_character,
                                        validator = function(value){
                                          c(
                                            chk_input_size(value, 0, 1, 1)
                                          )
                                        }),
    default_place_form = S7::new_property(S7::class_character,
                                          validator = function(value){
                                            chk_input_size(value, 0, 1, 1)
                                          }),
    notes = prop_notes(),
    note_xrefs = prop_xref(),
    
    GEDCOM = S7::new_property(
      S7::class_character, 
      getter = function(self){
        c(
          "0 HEAD",
          "1 GEDC",
          sprintf("2 VERS %s", self@gedcom_version),
          rep("1 SCHMA", length(self@ext_tags) > 0),
          sprintf("2 TAG %s", self@ext_tags),
          obj_to_ged(self@source) |> increase_level(by = 1),
          sprintf("1 DEST %s", self@destination),
          sprintf("1 DATE %s", obj_to_val(self@creation_date)),
          sprintf("2 TIME %s", obj_to_val(self@creation_time)),
          sprintf("1 SUBM %s", self@subm_xref),
          sprintf("1 COPR %s", self@gedcom_copyright),
          sprintf("1 LANG %s", self@default_language),
          rep("1 PLAC", length(self@default_place_form) > 0),
          sprintf("2 FORM %s", self@default_place_form),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_xrefs)
        )
      })
  ),
  validator = function(self){
    chk_input_parents(self@creation_time, "@creation_time", self@creation_date, "@creation_date")
  }
)


GedcomRecordsRaw <- S7::new_class(
  "GedcomRecordsRaw",
  parent = GedcomS7class,
  properties = list(
    SUBM = S7::class_list,
    INDI = S7::class_list,
    FAM = S7::class_list,
    SOUR = S7::class_list,
    REPO = S7::class_list,
    OBJE = S7::class_list,
    SNOTE = S7::class_list
  )
)


GedcomRecords <- S7::new_class(
  "GedcomRecords",
  parent = GedcomS7class,
  properties = list(
    # This serves as both a record of prefixes and order of records
    prefixes = S7::new_property(S7::class_character,
                                getter = function(self) self@prefixes,
                                setter = function(self, value){
                                  if(length(value) == 0){
                                    value <- c("U", "I", "F", "S", "R", "M", "N")
                                    names(value) <- names(GedcomRecordsRaw@properties)
                                  }
                                    
                                  self@prefixes <- value
                                  self
                                },
                                validator = function(value){
                                  num_types <- length(GedcomRecordsRaw@properties)
                                  c(
                                    chk_input_size(value, num_types, num_types, 0, 6),
                                    chk_input_choice(names(value), names(GedcomRecordsRaw@properties))
                                  )
                                }),
    # List of xrefs for each record type
    XREFS = S7::new_property(S7::class_character,
                             getter = function(self){
                               rec_types <- names(self@prefixes)
                               rec_xrefs <- lapply(rec_types, \(rec_type) 
                                                   names(S7::prop(self@RAW, rec_type)))
                               stats::setNames(rec_xrefs, rec_types)
                             }),
    XREFS_PRIV = S7::new_property(S7::class_character,
                                  getter = function(self){
                                    rec_types <- names(self@prefixes)
                                    priv <- lapply(rec_types, \(rec_type)
                                                   Filter(\(x) any(grepl("^1 RESN .*PRIVACY", x)),
                                                          S7::prop(self@RAW, rec_type)) |> names()
                                    )
                                    stats::setNames(priv, rec_types)
                                  }),
    XREFS_CONFID = S7::new_property(S7::class_character,
                                    getter = function(self){
                                      rec_types <- names(self@prefixes)
                                      priv <- lapply(rec_types, \(rec_type)
                                                     Filter(\(x) any(grepl("^1 RESN .*CONFIDENTIAL", x)),
                                                            S7::prop(self@RAW, rec_type)) |> names()
                                      )
                                      stats::setNames(priv, rec_types)
                                    }),
    XREFS_NEXT = S7::new_property(S7::class_character,
                                  getter = function(self){
                                    idx <- integer(length(self@prefixes))
                                    existing_xrefs <- unname(unlist(self@XREFS))
                                    for(i in seq_along(idx)){
                                      ref <- 1
                                      while(paste0("@", self@prefixes[i], ref, "@") %in% existing_xrefs){
                                        ref <- ref + 1
                                      }
                                      idx[i] <- ref
                                    }
                                    
                                    paste0("@", self@prefixes, idx, "@") |>
                                      stats::setNames(names(self@prefixes))
                                  }),
    RAW = GedcomRecordsRaw
  )
)

#' Create a GEDCOM object
#' 
#' You shouldn't need to use this directly to create new GEDCOM objects. Instead, use
#' `new_gedcom()` which populates relevant defaults.
#' 
#' @details
#' 
#' All information about records is contained in the `@records` property.
#' 
#' The `@prefixes` property is a named vector containing any alphanumeric string (up to 6 characters long) 
#' which will precede the number given to identify new records, of which there are currently 7 types:
#' 
#' Individual (INDI) 
#' Family (FAM)
#' Source (SOUR)
#' Repository (REPO)
#' Multimedia (OBJE)
#' Note (SNOTE)
#' Submitter (SUBM)
#' 
#' This vector must be of a particular length with specific names. For example:
#' c(SUBM = "U", INDI = "I", FAM = "F", SOUR = "S", REPO = "R", OBJE = "M", SNOTE = "N").
#' 
#' The order that these records appear in the vector will also dictate the order in which records 
#' will appear in the exported file.
#' 
#' @param update_change_dates Whether to automatically update change dates when updating records.
#' This happens when the record is pushed to the gedcom object.
#' @param add_creation_dates Whether to automatically add creation dates when creating records.
#' This happens when the record is pushed to the gedcom object.
#' @param header An S7 object whose properties contain information about the GEDCOM object as a whole.
#' @param records An S7 object whose properties contain information about all records. Do not edit
#' properties in capitals directly. See Details for more information. 
#' @returns An S7 object representing a GEDCOM file.
#' @tests
#' maximal <- test_path("maximal70.ged")
#' maximal <- withr::local_tempfile(lines = fix_maximal_header(maximal), 
#'                                  fileext = ".ged")
#' ged_raw <- readLines(maximal)
#' ged_parsed <- read_gedcom(maximal)
#' ged_parsed@records@prefixes <- c(FAM = "F", INDI = "I", OBJE = "M", REPO = "R", 
#'                                  SNOTE = "N", SOUR = "S", SUBM = "U")
#' ged_raw2 <- ged_parsed@GEDCOM
#' 
#' expect_equal(ged_raw, ged_raw2)
GedcomS7 <- S7::new_class(
  "GedcomS7",
  parent = GedcomS7class,
  properties = list(
    header = GedcomHeader,
    records = GedcomRecords,
    update_change_dates = prop_logical(default = FALSE),
    add_creation_dates = prop_logical(default = FALSE),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){

        ged <- self@header@GEDCOM
        
        for(rec_type in names(self@records@prefixes)){
          ged <- c(
            ged,
            unlist(S7::prop(self@records@RAW, rec_type))
          )
        }
        
        c(ged, "0 TRLR") |> 
          unname() |>
          prepare_gedcom_lines()
      }
    )
    
  )
)


#' Create a new GEDCOM object
#' 
#' @details
#' See documentation for `GedcomS7()` and `GedcomSource()` for object properties.
#'
#' @param my_language The primary language in which data will be stored. The language code should
#' adhere to BCP 47.
#'
#' @returns A minimal gedcom S7 object.
#' @export
new_gedcom <- function(my_language = "en"){
  
  sour <- GedcomSource(product_id = "gedcomS7",
                       product_name = "The 'gedcomS7' package for the R language",
                       business_name = "Jamie Lendrum",
                       emails = "jalendrum@gmail.com")
  
  head <- GedcomHeader(gedcom_version = "7.0",
           source = sour,
           creation_date = date_exact_current(),
           default_language = my_language)
  
  GedcomS7(header = head)

}

parse_gedcom_header <- function(hd_lines){
  
  sour <- NULL
  product_id <- find_ged_values(hd_lines, c("HEAD","SOUR"))
  
  if(length(product_id) == 1){
    
    sour <- GedcomSource(
      product_id = product_id,
      product_name = find_ged_values(hd_lines, c("HEAD","SOUR","NAME")),
      product_version = find_ged_values(hd_lines, c("HEAD","SOUR","VERS")),
      business_name = find_ged_values(hd_lines, c("HEAD","SOUR","CORP")),
      business_address = parse_address(hd_lines, c("HEAD","SOUR","CORP")),
      phone_numbers = find_ged_values(hd_lines, c("HEAD","SOUR","CORP","PHON")),
      emails = find_ged_values(hd_lines, c("HEAD","SOUR","CORP","EMAIL")),
      faxes = find_ged_values(hd_lines, c("HEAD","SOUR","CORP","FAX")),
      web_pages = find_ged_values(hd_lines, c("HEAD","SOUR","CORP","WWW")),
      data_name = find_ged_values(hd_lines, c("HEAD","SOUR","DATA")),
      data_pubdate = find_ged_values(hd_lines, c("HEAD","SOUR","DATA","DATE")),
      data_pubtime = find_ged_values(hd_lines, c("HEAD","SOUR","DATA","DATE","TIME")),
      data_copyright = find_ged_values(hd_lines, c("HEAD","SOUR","DATA","COPR"))
    )
  }
  
  head <- GedcomHeader(
    gedcom_version = find_ged_values(hd_lines, c("HEAD","GEDC","VERS")),
    ext_tags = find_ged_values(hd_lines, c("HEAD","SCHMA","TAG")),
    source = sour,
    destination = find_ged_values(hd_lines, c("HEAD","DEST")),
    creation_date = find_ged_values(hd_lines, c("HEAD","DATE")),
    creation_time = find_ged_values(hd_lines, c("HEAD","DATE","TIME")),
    subm_xref = find_ged_values(hd_lines, c("HEAD","SUBM")),
    gedcom_copyright = find_ged_values(hd_lines, c("HEAD","COPR")),
    default_language = find_ged_values(hd_lines, c("HEAD","LANG")),
    default_place_form = find_ged_values(hd_lines, c("HEAD","PLAC","FORM")),
    notes = parse_notes(hd_lines, "HEAD"),
    note_xrefs = find_ged_values(hd_lines, c("HEAD","SNOTE"))
  )
  
  GedcomS7(header = head)
  
}



# Source ------------------------------------------------------------------

raw_source_summary <- function(sour){
  exdent <- 20
  if(length(sour) == 0){
    to_console("Source system:", sour, exdent)
  } else {
    to_console_value_with_phrase("Source:", 
                                 sour@product_id, sour@product_version, 
                                 exdent)
    to_console("Source name:", sour@product_name, exdent)
  }
}

S7::method(summary, GedcomSource) <- function(object, ...){
  raw_source_summary(object)
}

# Header ------------------------------------------------------------------

raw_header_summary <- function(hd){
  exdent <- 20
  to_console("GEDCOM version:", hd@gedcom_version, exdent)
  to_console("Creation Date:", obj_to_val(hd@creation_date), exdent)
  to_console("Default Language:", hd@default_language, exdent)
  raw_source_summary(hd@source)
  cat("\n")
  to_console("Copyright:", hd@gedcom_copyright, exdent)
}

S7::method(summary, GedcomHeader) <- function(object, ...){
  raw_header_summary(object)
}

# Raw records ----------------------------------------------------------------

raw_record_summary <- function(raw){
  exdent <- 20
  to_console("Submitters:", length(raw@SUBM), exdent)
  to_console("Individuals:", length(raw@INDI), exdent)
  to_console("Families:", length(raw@FAM), exdent)
  to_console("Sources:", length(raw@SOUR), exdent)
  to_console("Repositories:", length(raw@REPO), exdent)
  to_console("Multimedia:", length(raw@OBJE), exdent)
  to_console("Notes:", length(raw@SNOTE), exdent)
}

S7::method(summary, GedcomRecordsRaw) <- function(object, ...){
  raw_record_summary(object)
}

S7::method(summary, GedcomRecords) <- function(object, ...){
  raw_record_summary(object@RAW)
}

# Gedcom ------------------------------------------------------------------

S7::method(summary, GedcomS7) <- function(object, ...){
  exdent <- 20
  
  cat("GEDCOM file summary:")
  cat("\n", "\n")
  raw_header_summary(object@header)
  cat("\n")
  raw_record_summary(object@records@RAW)
}



#' Create a GEDCOM source object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM HEAD.SOUR.
#' @export
GedcomSource <- S7::new_class(
  "GedcomSource",
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
    business_address = S7::new_property(S7::class_character | 
                                          S7::new_S3_class("gedcomS7::Address"),
                                        validator = function(value){
                                          chk_input_size(value, 0, 1, 1)
                                        }),
    phone_numbers = S7::new_property(S7::class_character,
                                     validator = function(value){
                                       chk_input_size(value, min_val = 1)
                                     }),
    emails = S7::new_property(S7::class_character,
                              validator = function(value){
                                chk_input_size(value, min_val = 1)
                              }),
    faxes = S7::new_property(S7::class_character,
                             validator = function(value){
                               chk_input_size(value, min_val = 1)
                             }),
    web_pages = S7::new_property(S7::class_character,
                                 validator = function(value){
                                   chk_input_size(value, min_val = 1)
                                 }),
    data_name = S7::new_property(S7::class_character,
                                 validator = function(value){
                                   chk_input_size(value, 0, 1, 1)
                                 }),
    data_pubdate = S7::new_property(S7::class_character | 
                                      S7::new_S3_class("gedcomS7::DateExact"),
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
    
    c_as_ged = S7::new_property(
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
#' @return An S7 object representing a GEDCOM HEADER.
#' @export
GedcomHeader <- S7::new_class(
  "GedcomHeader",
  abstract = TRUE,
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
                                     default = date_exact_current(),
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
    subm_xref = S7::new_property(S7::class_character,
                                 validator = function(value){
                                   c(
                                     chk_input_size(value, 0, 1),
                                     chk_input_pattern(value, reg_xref(TRUE))
                                   )
                                 }),
    gedcom_copyright = S7::new_property(S7::class_character,
                                        validator = function(value){
                                          chk_input_size(value, 0, 1, 1)
                                        }),
    default_language = S7::new_property(S7::class_character,
                                        validator = function(value){
                                          c(
                                            chk_input_size(value, 0, 1, 1)
                                            # chk_input_choice(value, val_languages()),#TODO
                                          )
                                        }),
    default_place_form = S7::new_property(S7::class_character,
                                          validator = function(value){
                                            chk_input_size(value, 0, 1, 1)
                                          }),
    notes = S7::new_property(S7::class_list | 
                               S7::new_S3_class("gedcomS7::Note") | 
                               S7::class_character,
                             validator = function(value){
                               chk_input_S7classes(value, Note, ".+")
                             }),
    note_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    
    c_hd_as_ged = S7::new_property(
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

#' Create a GEDCOM object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM file.
#' @export
#' @tests
#' skip_if_offline(host = "gedcom.io")
#' ged_raw <- readLines("https://gedcom.io/testfiles/gedcom70/maximal70.ged")
#' ged_parsed <- read_gedcom("https://gedcom.io/testfiles/gedcom70/maximal70.ged")
#' ged_parsed@xref_prefixes <- c(fam = "F", indi = "I", media = "M", repo = "R", 
#'                                note = "N", sour = "S", subm = "U")
#' ged_raw2 <- ged_parsed@c_as_ged
#' 
#' expect_equal(ged_raw, ged_raw2)
GedcomS7 <- S7::new_class(
  "GedcomS7",
  parent = GedcomHeader,
  properties = list(
    update_change_dates = S7::new_property(S7::class_logical, default = FALSE,
                                           validator = function(value){
                                             chk_input_size(value, 1, 1)
                                           }),
    add_creation_dates = S7::new_property(S7::class_logical, default = FALSE,
                                          validator = function(value){
                                            chk_input_size(value, 1, 1)
                                          }),
    
    # Records
    subm = S7::class_list,
    indi = S7::class_list,
    fam = S7::class_list,
    sour = S7::class_list,
    repo = S7::class_list,
    media = S7::class_list,
    note = S7::class_list,
    
    # This serves as both a record of prefixes and order of records
    xref_prefixes = S7::new_property(S7::class_character,
                                     default = c(subm = "U", indi = "I", fam = "F", sour = "S", 
                                                 repo = "R", media = "M", note = "N"),
                                     validator = function(value){
                                       c(
                                         chk_input_size(value, 7, 7, 0, 6),
                                         chk_input_choice(names(value), c("indi","fam","sour","subm",
                                                                          "repo","media","note"))
                                       )
                                     }),
    
    # List of xrefs for each record type
    c_xrefs = S7::new_property(S7::class_list,
                               getter = function(self){
                                 rec_types <- names(self@xref_prefixes)
                                 rec_xrefs <- lapply(rec_types, \(rec_type) names(S7::prop(self, rec_type)))
                                 stats::setNames(rec_xrefs, rec_types)
                               }),
    
    c_next_xref = S7::new_property(S7::class_character,
                                   getter = function(self){
                                     idx <- integer(7L)
                                     existing_xrefs <- unname(unlist(self@c_xrefs))
                                     for(i in seq_along(idx)){
                                       ref <- 1
                                       while(paste0("@", self@xref_prefixes[i], ref, "@") %in% existing_xrefs){
                                         ref <- ref + 1
                                       }
                                       idx[i] <- ref
                                     }
                                     
                                     paste0("@", self@xref_prefixes, idx, "@") |>
                                       stats::setNames(names(self@xref_prefixes))
                                   }),
    
    c_as_ged = S7::new_property(
      S7::class_character, 
      getter = function(self){
        
        hd <- self@c_hd_as_ged
        tr <- "0 TRLR"
        
        c(
          hd,
          unlist(S7::prop(self, names(self@xref_prefixes)[1])),
          unlist(S7::prop(self, names(self@xref_prefixes)[2])),
          unlist(S7::prop(self, names(self@xref_prefixes)[3])),
          unlist(S7::prop(self, names(self@xref_prefixes)[4])),
          unlist(S7::prop(self, names(self@xref_prefixes)[5])),
          unlist(S7::prop(self, names(self@xref_prefixes)[6])),
          unlist(S7::prop(self, names(self@xref_prefixes)[7])),
          tr
        ) |> unname() |> 
          prepare_gedcom_lines()
      }
    )
    
  )
)


#' Create a new gedcom object
#'
#' @param my_language The primary language in which data will be stored. The language code should
#' adhere to BCP 47.
#'
#' @return A minimal gedcom S7 object.
#' @export
new_gedcom <- function(my_language = "en"){
  
  sour <- GedcomSource(product_id = "https://github.com/jl5000/gedcomS7",
                              product_name = "The 'gedcomS7' package for the R language",
                              business_name = "Jamie Lendrum",
                              emails = "jalendrum@gmail.com")
  
  GedcomS7(gedcom_version = "7.0",
                 source = sour,
                 creation_date = date_exact_current(),
                 default_language = my_language)
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
      data_pubdate = find_ged_values(hd_lines, c("HEAD","SOUR","DATA","DATE")) |> toupper(),
      data_pubtime = find_ged_values(hd_lines, c("HEAD","SOUR","DATA","DATE","TIME")),
      data_copyright = find_ged_values(hd_lines, c("HEAD","SOUR","DATA","COPR"))
    )
  }
  
  GedcomS7(
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
    notes = parse_notes(hd_lines, "HEAD"),
    note_xrefs = find_ged_values(hd_lines, c("HEAD","SNOTE"))
  )
  
}


#' @include cls_validators.R
NULL

#' @include cls_locations.R cls_dates.R
class_gedcom_source <- S7::new_class("class_source_system",
                                     properties = list(
                                       product_id = S7::class_character,
                                       product_name = S7::class_character,
                                       product_version = S7::class_character,
                                       business_name = S7::class_character,
                                       business_address = S7::new_property(S7::new_union(NULL, class_address)),
                                       phone_numbers = S7::class_character,
                                       emails = S7::class_character,
                                       faxes = S7::class_character,
                                       web_pages = S7::class_character,
                                       data_name = S7::class_character,
                                       data_pubdate = S7::new_property(S7::new_union(NULL, class_date_exact, S7::class_character)),
                                       data_pubtime = S7::new_property(S7::new_union(NULL, class_time, S7::class_character)),
                                       data_copyright = S7::class_character,
                                       
                                       as_ged = S7::new_property(
                                         S7::class_character,
                                         getter = function(self){
                                           c(
                                             sprintf("1 SOUR %s", self@product_id),
                                             sprintf("2 VERS %s", self@product_version),
                                             sprintf("2 NAME %s", self@product_name),
                                             sprintf("2 CORP %s", self@business_name),
                                             obj_to_ged(self@business_address) |> increase_level(by = 3),
                                             sprintf("3 PHON %s", self@phone_numbers),
                                             sprintf("3 EMAIL %s", self@emails),
                                             sprintf("3 FAX %s", self@faxes),
                                             sprintf("3 WWW %s", self@web_pages),
                                             sprintf("2 DATA %s", self@data_name),
                                             sprintf("3 DATE %s", date_to_val(self@data_pubdate)),
                                             sprintf("4 TIME %s", self@data_pubtime),
                                             sprintf("3 COPR %s", self@data_copyright)
                                           )
                                         }
                                       ),
                                       validator = function(self){
                                         c(
                                           chk_input_size(self@product_id, "@product_id", 1, 1, 1),
                                           chk_input_size(self@product_name, "@product_name", 0, 1, 1),
                                           chk_input_size(self@product_version, "@product_version", 0, 1),
                                           chk_input_pattern(self@product_version,  "@product_version", "^\\d{1,3}\\.\\d{1,3}(\\.\\d{1,3}(\\.\\d{1,3})?)?$"),
                                           chk_input_size(self@business_name, "@business_name", 0, 1, 1),
                                           chk_input_size(self@business_address, "@business_address", 0, 1),
                                           chk_input_size(self@phone_numbers, "@phone_numbers", min_char = 1),
                                           chk_input_size(self@emails, "@emails", min_char = 1),
                                           chk_input_size(self@faxes, "@faxes", min_char = 1),
                                           chk_input_size(self@web_pages, "@web_pages", min_char = 1),
                                           chk_input_size(self@data_name, "@data_name", 0, 1, 1),
                                           chk_input_size(self@data_pubdate, "@data_pubdate", 0, 1),
                                           chk_input_pattern(self@data_pubdate, "@data_pubdate", reg_date_exact()),
                                           chk_input_size(self@data_pubtime, "@data_pubtime", 0, 1),
                                           #TODO: time pattern
                                           chk_input_size(self@data_copyright, "@data_copyright", 0, 1, 1)
                                         )
                                       }
                                     ))

#' @include cls_common.R cls_dates.R
class_gedcomS7 <- S7::new_class("class_gedcomS7",
                                properties = list(
                                  gedcom_version = S7::class_character,
                                  ext_tags = S7::class_character,
                                  source_details = class_gedcom_source,
                                  receiving_system = S7::class_character,
                                  creation_date = S7::new_property(S7::new_union(NULL, class_date_exact, S7::class_character)),
                                  creation_time = S7::new_property(S7::new_union(NULL, class_time, S7::class_character)),
                                  xref_subm = S7::class_character,
                                  gedcom_copyright = S7::class_character,
                                  language = S7::class_character,
                                  default_place_form = S7::class_character,
                                  content_description = S7::new_property(S7::new_union(NULL, class_note, S7::class_character)),
                                  
                                  update_change_dates = S7::new_property(S7::class_logical, default = FALSE),
                                  add_creation_dates = S7::new_property(S7::class_logical, default = FALSE),
                                  
                                  # Records
                                  subm = S7::class_list,
                                  indi = S7::class_list,
                                  famg = S7::class_list,
                                  sour = S7::class_list,
                                  repo = S7::class_list,
                                  media = S7::class_list,
                                  note = S7::class_list,
                                  
                                  
                                  as_ged = S7::new_property(
                                    S7::class_character, 
                                    getter = function(self){
                                      
                                      hd <- c(
                                        "0 HEAD",
                                        "1 GEDC",
                                        sprintf("2 VERS %s", self@gedcom_version),
                                        rep("1 SCHMA", length(self@ext_tags) > 0),
                                        sprintf("2 TAG %s", self@ext_tags),
                                        obj_to_ged(self@source_details),
                                        sprintf("1 DEST %s", self@receiving_system),
                                        sprintf("1 DATE %s", date_to_val(self@creation_date)),
                                        sprintf("2 TIME %s", self@creation_time),
                                        sprintf("1 SUBM %s", self@xref_subm),
                                        sprintf("1 COPR %s", self@gedcom_copyright),
                                        sprintf("1 LANG %s", self@language),
                                        rep("1 PLAC", length(self@default_place_form) > 0),
                                        sprintf("2 FORM %s", self@default_place_form),
                                        obj_to_ged(self@content_description)
                                      )
                                      
                                      c(
                                        hd,
                                        unlist(self@subm),
                                        unlist(self@indi),
                                        unlist(self@fam),
                                        unlist(self@sour),
                                        unlist(self@repo),
                                        unlist(self@media),
                                        unlist(self@note),
                                        "0 TRLR"
                                      ) |> unname()
                                    }
                                  )
                                  
                                ),
                                validator = function(self){
                                  c(
                                    
                                    chk_input_size(self@creation_date, "@creation_date", 0, 1),
                                    chk_input_pattern(self@creation_date, "@creation_date", reg_date_exact()),
                                    chk_input_size(self@creation_time, "@creation_time", 0, 1, 4, 11), # different from spec
                                    chk_input_pattern(self@creation_time, "@creation_time", reg_time()),
                                    chk_input_size(self@language, "@language", 0, 1, 1, 15),
                                    chk_input_choice(self@language, "@language", val_languages()),
                                    chk_input_size(self@file_name, "@file_name", 0, 1, 5, 248),
                                    chk_input_size(self@gedcom_copyright, "@gedcom_copyright", 0, 1, 1, 248),
                                    chk_input_size(self@content_description, "@content_description", 0, 1, 1, 248),
                                    chk_input_size(self@xref_prefixes, "@xref_prefixes", 6, 6, 0, 6),
                                    chk_input_choice(names(self@xref_prefixes), "@xref_prefixes names", c("indi","famg","sour","repo","media","note"))
                                    # TODO: names and values must be unique
                                    # Check all xrefs point to a record
                                    #chk_xref_pointers_valid(self)
                                  )
                                }
)


#' Create a new gedcom object
#'
#' @param my_language The primary language in which data will be stored.
#'
#' @return A minimal gedcom S7 object.
#' @export
new_gedcomS7 <- function(my_language = "en"){
  
  sour <- class_gedcom_source(product_id = "https://github.com/jl5000/gedcomS7",
                              product_name = "The 'gedcomS7' package for the R language",
                              business_name = "Jamie Lendrum",
                              emails = "jalendrum@gmail.com")
  
  class_gedcomS7(gedcom_version = "7.0.11",
                 source_details = sour,
                 creation_date = date_exact_current(),
                 language = my_language)
}


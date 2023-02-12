#' @include utils_at.R cls_dates.R cls_locations.R cls_record.R cls_validators.R
NULL

class_gedcomR7 <- R7::new_class("class_gedcomR7",
                                properties = list(
                                  update_change_dates = R7::new_property(R7::class_logical, default = FALSE),
                                  
                                  gedcom_version = R7::new_property(getter = function(self) "5.5.5"),
                                  gedcom_form = R7::new_property(getter = function(self) "LINEAGE-LINKED"),
                                  character_encoding = R7::new_property(getter = function(self) "UTF-8"),
                                  system_id = R7::class_character,
                                  product_name = R7::class_character,
                                  product_version = R7::class_character,
                                  business_name = R7::class_character,
                                  business_address = R7::new_property(R7::new_union(NULL, class_address)),
                                  source_data_name = R7::class_character,
                                  source_data_pubdate = R7::new_property(R7::new_union(NULL, class_date_exact, R7::class_character)),
                                  source_data_copyright = R7::class_character,
                                  receiving_system = R7::class_character,
                                  creation_date = R7::new_property(R7::new_union(NULL, class_date_exact, R7::class_character)),
                                  creation_time = R7::class_character,
                                  language = R7::class_character,
                                  xref_subm = R7::class_character,
                                  file_name = R7::class_character,
                                  gedcom_copyright = R7::class_character,
                                  content_description = R7::class_character,
                                  
                                  # This serves as both a record of prefixes and order of records
                                  xref_prefixes = R7::new_property(R7::class_character,
                                                                   default = c(indi = "I", famg = "F", sour = "S", 
                                                                               repo = "R", media = "M", note = "N")),
                                  
                                  # Records
                                  subm = class_subm,
                                  indi = R7::class_list,
                                  famg = R7::class_list,
                                  sour = R7::class_list,
                                  repo = R7::class_list,
                                  media = R7::class_list,
                                  note = R7::class_list,
                                  
                                  
                                  # List of xrefs for each record type
                                  xrefs = R7::new_property(R7::class_list,
                                                           getter = function(self){
                                                             rec_types <- names(self@xref_prefixes)
                                                             rec_xrefs <- lapply(rec_types, \(rec_type) names(R7::prop(self, rec_type)))
                                                             setNames(rec_xrefs, rec_types)
                                                             rec_xrefs
                                                           }),

                                  next_xref = R7::new_property(R7::class_character,
                                                               getter = function(self){
                                                                 idx <- integer(6L)
                                                                 existing_xrefs <- unname(unlist(self@xrefs))
                                                                 for(i in seq_along(idx)){
                                                                   ref <- 1
                                                                   while(paste0("@", self@xref_prefixes[i], ref, "@") %in% existing_xrefs){
                                                                     ref <- ref + 1
                                                                   }
                                                                   idx[i] <- ref
                                                                 }
                                                                 
                                                                 paste0("@", self@xref_prefixes, idx, "@") |>
                                                                   setNames(names(self@xref_prefixes))
                                                               }),
                                  
                                  as_ged = R7::new_property(
                                    R7::class_character, 
                                    getter = function(self){
                                      
                                      hd <- c(
                                        "0 HEAD",
                                        "1 GEDC",
                                        sprintf("2 VERS %s", self@gedcom_version),
                                        sprintf("2 FORM %s", self@gedcom_form),
                                        sprintf("3 VERS %s", self@gedcom_version),
                                        sprintf("1 CHAR %s", self@character_encoding),
                                        sprintf("1 SOUR %s", self@system_id),
                                        sprintf("2 NAME %s", self@product_name),
                                        sprintf("2 VERS %s", self@product_version),
                                        sprintf("2 CORP %s", self@business_name),
                                        obj_to_ged(self@business_address) |> increase_level(by = 3),
                                        sprintf("2 DATA %s", self@source_data_name),
                                        sprintf("3 DATE %s", date_to_val(self@source_data_pubdate)),
                                        sprintf("3 COPR %s", self@source_data_copyright),
                                        sprintf("1 DEST %s", self@receiving_system),
                                        sprintf("1 DATE %s", date_to_val(self@creation_date)),
                                        sprintf("2 TIME %s", self@creation_time),
                                        sprintf("1 LANG %s", self@language),
                                        sprintf("1 SUBM %s", self@subm@xref),
                                        sprintf("1 FILE %s", self@file_name),
                                        sprintf("1 COPR %s", self@gedcom_copyright),
                                        sprintf("1 NOTE %s", self@content_description)
                                      )
                                      
                                      c(
                                        hd,
                                        obj_to_ged(self@subm),
                                        unlist(R7::prop(self, names(self@xref_prefixes)[1])),
                                        unlist(R7::prop(self, names(self@xref_prefixes)[2])),
                                        unlist(R7::prop(self, names(self@xref_prefixes)[3])),
                                        unlist(R7::prop(self, names(self@xref_prefixes)[4])),
                                        unlist(R7::prop(self, names(self@xref_prefixes)[5])),
                                        unlist(R7::prop(self, names(self@xref_prefixes)[6])),
                                        "0 TRLR"
                                      ) |> unname()
                                    }
                                  )
                                  
                                ),
                                validator = function(self){
                                  c(
                                    chk_input_size(self@system_id, "@system_id", 1, 1, 1, 20),
                                    chk_input_size(self@product_name, "@product_name", 0, 1, 1, 90),
                                    chk_input_size(self@product_version, "@product_version", 0, 1, 3, 15),
                                    chk_input_pattern(self@product_version,  "@product_version", "^\\d{1,3}\\.\\d{1,3}(\\.\\d{1,3}(\\.\\d{1,3})?)?$"),
                                    chk_input_size(self@business_name, "@business_name", 0, 1, 1, 90),
                                    chk_input_size(self@business_address, "@business_address", 0, 1),
                                    chk_input_size(self@source_data_name, "@source_data_name", 0, 1, 1, 90),
                                    chk_input_size(self@source_data_pubdate, "@source_data_pubdate", 0, 1),
                                    chk_input_pattern(self@source_data_pubdate, "@source_data_pubdate", reg_date_exact()),
                                    chk_input_size(self@source_data_copyright, "@source_data_copyright", 0, 1, 1, 248),
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
#' @param my_name Your name, as creator/submitter.
#' @param my_language The primary language in which data will be stored.
#'
#' @return A minimal gedcom R7 object.
#' @export
new_gedcomR7 <- function(my_name = unname(Sys.info()["user"]),
                         my_language = "English"){
  class_gedcomR7(system_id = "gedcomR7",
                 product_name = "The 'gedcomR7' package for the R language",
                 business_name = "Jamie Lendrum",
                 business_address = class_address(emails = "jalendrum@gmail.com"),
                 creation_date = date_exact_current(),
                 language = my_language,
                 subm = class_subm(xref = "@U1@", name = my_name),
                 xref_subm = "@U1@"
  )
}


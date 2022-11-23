
#child has additional properties that can be set (new)

# if creating a new obj from scratch, certain properties need to be preset (parent) class_new - take out presets
# if importing, all properties can be open (child)  class_general - add in properties that were preset

class_gedcomR7 <- R7::new_class("class_gedcomR7",
                            properties = list(
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
                              
                              # Records
                              subm = class_record_subm,
                              indi = R7::class_list,
                              famg = R7::class_list,
                              sour = R7::class_list,
                              repo = R7::class_list,
                              media = R7::class_list,
                              note = R7::class_list,
                              
                              xref_prefixes = R7::new_property(R7::class_character,
                                                               default = rep("", 6) |> 
                                                                 purrr::set_names("indi","famg","sour",
                                                                                  "repo","media","note")),
                              
                              xrefs <- R7::new_property(R7::class_list,
                                                        getter = function(self){
                                                          list(self@indi, self@famg, self@sour, 
                                                               self@repo, self@media, self@note) |>
                                                          purrr::map() |> #TODO: Figure this out
                                                            purrr::map(~ .x@xref) |>
                                                            purrr::set_names("indi","famg","sour",
                                                                             "repo","media","note")
                                                        }),
                              
                              # indi_xrefs = R7::new_property(R7::class_character,
                              #                               getter = function(self) purrr::map_chr(self@indi, ~ .x@xref)),
                              # famg_xrefs = R7::new_property(R7::class_character,
                              #                               getter = function(self) purrr::map_chr(self@famg, ~ .x@xref)),
                              # sour_xrefs = R7::new_property(R7::class_character,
                              #                               getter = function(self) purrr::map_chr(self@sour, ~ .x@xref)),
                              # repo_xrefs = R7::new_property(R7::class_character,
                              #                               getter = function(self) purrr::map_chr(self@repo, ~ .x@xref)),
                              # media_xrefs = R7::new_property(R7::class_character,
                              #                               getter = function(self) purrr::map_chr(self@media, ~ .x@xref)),
                              # note_xrefs = R7::new_property(R7::class_character,
                              #                               getter = function(self) purrr::map_chr(self@note, ~ .x@xref)),
                              
                              # next xref
                              next_xref = R7::new_property(R7::class_character,
                                                           getter = function(self){
                                                             idx <- integer(6L)
                                                             for(i in seq_along(idx)){
                                                               if(length(self@xrefs[[i]]) == 0){
                                                                 idx[i] <- 1
                                                               } else {
                                                                 existing_refs <- self@xrefs[[i]] |>
                                                                   stringr::str_remove_all("@") |> 
                                                                   stringr::str_remove_all(unname(self@xref_prefixes[i])) |> #TODO: Needs work if prefixes is ""
                                                                   as.integer()
                                                                 
                                                                 available_refs <- seq_len(max(existing_refs) + 1) |>
                                                                   dplyr::setdiff(existing_refs)
                                                                 
                                                                 idx[i] <- available_refs[1]
                                                               }
                                                             }
             
                                                             paste0("@", self@xref_prefixes, idx, "@")
                                                           }),
                              
                              as_df = R7::new_property(
                                R7::class_data.frame, 
                                getter = function(self){

                                  hd <- dplyr::bind_rows(
                                    df_rows(level = 0, tag = "HEAD", value = ""),
                                    df_rows(level = 1, tag = "GEDC", value = ""),
                                    df_rows(level = 2, tag = "VERS", value = self@gedcom_version),
                                    df_rows(level = 2, tag = "FORM", value = self@gedcom_form),
                                    df_rows(level = 3, tag = "VERS", value = self@gedcom_version),
                                    df_rows(level = 1, tag = "CHAR", value = self@character_encoding),
                                    df_rows(level = 1, tag = "SOUR", value = self@system_id),
                                    df_rows(level = 2, tag = "NAME", value = self@product_name),
                                    df_rows(level = 2, tag = "VERS", value = self@product_version),
                                    df_rows(level = 2, tag = "CORP", value = self@business_name),
                                    obj_to_df(self@business_address, level_inc = 3),
                                    df_rows(level = 2, tag = "DATA", value = self@source_data_name),
                                    date_to_df(self@source_data_pubdate, level_inc = 3),
                                    df_rows(level = 3, tag = "COPR", value = self@source_data_copyright),
                                    df_rows(level = 1, tag = "DEST", value = self@receiving_system),
                                    date_to_df(self@creation_date, level_inc = 1),
                                    df_rows(level = 2, tag = "TIME", value = self@creation_time),
                                    df_rows(level = 1, tag = "LANG", value = self@language),
                                    df_rows(level = 1, tag = "SUBM", value = self@subm@xref),
                                    df_rows(level = 1, tag = "FILE", value = self@file_name),
                                    df_rows(level = 1, tag = "COPR", value = self@gedcom_copyright),
                                    df_rows(level = 1, tag = "NOTE", value = self@content_description)
                                  ) |> 
                                    dplyr::mutate(record = "HD", .before = 1)
                                    
                                    tr <- df_rows(record = "TR", level = 0, tag = "TRLR", value = "")
                                  
                                    dplyr::bind_rows(
                                      hd,
                                      obj_to_df(self@subm, level_inc = 0),
                                      lst_to_df(self@indi, level_inc = 0),
                                      lst_to_df(self@famg, level_inc = 0),
                                      lst_to_df(self@sour, level_inc = 0),
                                      lst_to_df(self@repo, level_inc = 0),
                                      lst_to_df(self@media, level_inc = 0),
                                      lst_to_df(self@note, level_inc = 0),
                                      tr
                                    )
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
                                chk_input_size(self@creation_time, "@creation_time", 0, 1, 7, 12),
                                chk_input_pattern(self@creation_time, "@creation_time", paste0("^\\d{1,2}:\\d\\d:\\d\\d$|",
                                                                                               "^\\d{1,2}:\\d\\d:\\d\\d.\\d\\d$")),
                                chk_input_size(self@language, "@language", 0, 1, 1, 15),
                                chk_input_choice(self@language, "@language", val_languages()),
                                chk_input_size(self@file_name, "@file_name", 0, 1, 5, 248),
                                chk_input_size(self@gedcom_copyright, "@gedcom_copyright", 0, 1, 1, 248),
                                chk_input_size(self@content_description, "@content_description", 0, 1, 1, 248),
                                chk_input_R7classes(self@indi, "@indi", class_record_indi),
                                chk_input_R7classes(self@famg, "@famg", class_record_famg),
                                chk_input_R7classes(self@sour, "@sour", class_record_sour),
                                chk_input_R7classes(self@repo, "@repo", class_record_repo),
                                chk_input_R7classes(self@media, "@media", class_record_media),
                                chk_input_R7classes(self@note, "@note", class_record_note),
                                chk_input_size(self@xref_prefixes, "@xref_prefixes", 0, 1, 6, 6) # TODO
                              )
                            }
)

new_gedcomR7 <- function(submitter_details = class_record_subm(xref = "@U1@"),
                         language = "English",
                         xref_prefixes = c(indi = "I", famg = "F", sour = "S", 
                                           repo = "R", media = "M", note = "N")){
  class_gedcomR7(system_id = "gedcomR7",
                 product_name = "The 'gedcomR7' package for the R language",
                 business_name = "Jamie Lendrum",
                 business_address = class_address(emails = "jalendrum@gmail.com"),
                 creation_date = date_exact_current(),
                 language = language,
                 subm = submitter_details,
                 xref_prefixes = xref_prefixes
                 )
}

R7::method(print, class_gedcomR7) <- function(x, ...){
  
  eol <- "\n"
  # this is the longest string
  title_width <- nchar("Source system version:") + 2
  
  paste("GEDCOM file summary:", eol, eol,
        stringr::str_pad("Submitter:", title_width, "right"), x@subm@name, eol, 
        stringr::str_pad("Description:", title_width, "right"), x@content_description, eol,
        stringr::str_pad("Language:", title_width, "right"), x@language, eol,
        stringr::str_pad("Character set:", title_width, "right"), x@character_encoding, eol, eol,
        
        stringr::str_pad("Copyright:", title_width, "right"), x@gedcom_copyright, eol, eol,
        
        stringr::str_pad("Source system:", title_width, "right"), x@system_id, eol,
        stringr::str_pad("Source system version:", title_width, "right"), x@product_version, eol,
        stringr::str_pad("Product name:", title_width, "right"), x@product_name, eol,
        stringr::str_pad("Product source:", title_width, "right"), x@business_name, eol, eol,
        
        stringr::str_pad("Individuals:", title_width, "right"), length(x@indi), eol,
        stringr::str_pad("Families:", title_width, "right"), length(x@famg), eol,
        stringr::str_pad("Multimedia objects:", title_width, "right"), length(x@media), eol, 
        stringr::str_pad("Notes:", title_width, "right"), length(x@note), eol,
        stringr::str_pad("Sources:", title_width, "right"), length(x@sour), eol,
        stringr::str_pad("Repositories:", title_width, "right"), length(x@repo), eol 
  ) |> cat()
  
}

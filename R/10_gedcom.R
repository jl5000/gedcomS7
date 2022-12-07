
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
                              
                              indi = R7::new_property(
                                R7::class_list,
                                setter = function(self, value){
                                  self@indi <- value
                                  names(self@indi) <- purrr::map_chr(self@indi, \(rec) rec@xref)
                                  refresh_famg_members(self)
                                }),
                              
                              famg = R7::new_property(
                                R7::class_list,
                                setter = function(self, value){
                                  self@famg <- value
                                  names(self@famg) <- purrr::map_chr(self@famg, \(rec) rec@xref)
                                  refresh_indi_links(self)
                                }),
                              
                              sour = R7::new_property(
                                R7::class_list,
                                setter = function(self, value){
                                  self@sour <- value
                                  names(self@sour) <- purrr::map_chr(self@sour, \(rec) rec@xref)
                                  self
                                }),
                              
                              repo = R7::new_property(
                                R7::class_list,
                                setter = function(self, value){
                                  self@repo <- value
                                  names(self@repo) <- purrr::map_chr(self@repo, \(rec) rec@xref)
                                  self
                                }),
                              
                              media = R7::new_property(
                                R7::class_list,
                                setter = function(self, value){
                                  self@media <- value
                                  names(self@media) <- purrr::map_chr(self@media, \(rec) rec@xref)
                                  self
                                }),
                              
                              note = R7::new_property(
                                R7::class_list,
                                setter = function(self, value){
                                  self@note <- value
                                  names(self@note) <- purrr::map_chr(self@note, \(rec) rec@xref)
                                  self
                                }),
                              
                              get_indi = R7::new_property(
                                R7::class_list,
                                getter = function(self){
                                  self@indi  
                                }
                              ),
                              
                              get_famg = R7::new_property(
                                R7::class_list,
                                getter = function(self){
                                  self@famg  
                                }
                              ),
                              
                              # This serves as both a record of prefixes and order of records
                              xref_prefixes = R7::new_property(R7::class_character,
                                                               default = c(indi = "I", famg = "F", sour = "S", 
                                                                           repo = "R", media = "M", note = "N")),
                              
                              # List of xrefs for each record type
                              xrefs = R7::new_property(R7::class_list,
                                                        getter = function(self){
                                                          purrr::map(1:6, ~ R7::prop(self, names(self@xref_prefixes)[.x])) |> 
                                                            purrr::map(\(rec_list) purrr::map_chr(rec_list, \(rec) rec@xref)) |>
                                                            purrr::set_names(names(self@xref_prefixes))
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
                                                               purrr::set_names(names(self@xref_prefixes))
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
                                      lst_to_df(R7::prop(self, names(self@xref_prefixes)[1]), level_inc = 0),
                                      lst_to_df(R7::prop(self, names(self@xref_prefixes)[2]), level_inc = 0),
                                      lst_to_df(R7::prop(self, names(self@xref_prefixes)[3]), level_inc = 0),
                                      lst_to_df(R7::prop(self, names(self@xref_prefixes)[4]), level_inc = 0),
                                      lst_to_df(R7::prop(self, names(self@xref_prefixes)[5]), level_inc = 0),
                                      lst_to_df(R7::prop(self, names(self@xref_prefixes)[6]), level_inc = 0),
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
                                chk_input_size(self@xref_prefixes, "@xref_prefixes", 6, 6, 1, 1),
                                chk_input_choice(names(self@xref_prefixes), "@xref_prefixes names", c("indi","famg","sour","repo","media","note"))#,
                                # TODO: names and values must be unique
                                # Check all xrefs point to a record
                                #chk_xref_pointers_valid(self)
                              )
                            }
)

new_gedcomR7 <- function(my_name = unname(Sys.info()["user"]),
                         my_language = "English"){
  class_gedcomR7(system_id = "gedcomR7",
                 product_name = "The 'gedcomR7' package for the R language",
                 business_name = "Jamie Lendrum",
                 business_address = class_address(emails = "jalendrum@gmail.com"),
                 creation_date = date_exact_current(),
                 language = my_language,
                 subm = class_record_subm(xref = "@U1@", name = my_name),
                 xref_subm = "@U1@"
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

refresh_famg_members <- function(x){
  
  x |>
    refresh_famg_spou() |>
    refresh_famg_chil()
  
}

refresh_famg_spou <- function(x){
  
  xdf <- x@as_df
  
  spou_links <- dplyr::filter(xdf, level == 1,
                              tag %in% c("HUSB","WIFE","FAMS")) |>
    dplyr::mutate(famg = dplyr::if_else(tag == "FAMS", value,  record)) |>
    dplyr::mutate(indi = dplyr::if_else(tag == "FAMS", record, value)) |>
    dplyr::mutate(pair = paste(famg, indi)) |>
    dplyr::add_count(pair) |>
    dplyr::filter(n < 2)
  
  for(i in seq_len(nrow(spou_links))){
    tag <- spou_links$tag[i]
    indi <- spou_links$indi[i]
    famg <- spou_links$famg[i]
    husb <- x@get_famg[[famg]]@husb_xref
    wife <- x@get_famg[[famg]]@wife_xref
    
    if(tag == "FAMS"){ # add spouse to famg
      
      if(length(c(husb, wife)) == 0){
        # use sex as determinant
        if(x@get_indi[[indi]]@sex == "M"){
          x@get_famg[[famg]]@husb_xref <- indi
        } else {
          x@get_famg[[famg]]@wife_xref <- indi
        }
        
      } else if(length(husb) == 0) {
        x@get_famg[[famg]]@husb_xref <- indi
      } else {
        x@get_famg[[famg]]@wife_xref <- indi
      }
      
    } else { # remove spouse from famg
      
      if(husb == indi){
        x@get_famg[[famg]]@husb_xref <- character()
      } else if (wife == indi){
        x@get_famg[[famg]]@wife_xref <- character()
      }
      
    }

  }
  
  x
}

refresh_famg_chil <- function(x){
  
  xdf <- x@as_df
  
  chil_links <- dplyr::filter(xdf, level == 1,
                              tag %in% c("CHIL","FAMC")) |>
    dplyr::mutate(famg = dplyr::if_else(tag == "FAMC", value,  record)) |>
    dplyr::mutate(indi = dplyr::if_else(tag == "FAMC", record, value)) |>
    dplyr::mutate(pair = paste(famg, indi)) |>
    dplyr::add_count(pair) |>
    dplyr::filter(n < 2)
  
  for(i in seq_len(nrow(chil_links))){
    tag <- chil_links$tag[i]
    indi <- chil_links$indi[i]
    famg <- chil_links$famg[i]
    chil <- x@get_famg[[famg]]@chil_xref
    
    if(tag == "FAMC"){ # add child to famg
      
      x@get_famg[[famg]]@chil_xref <- c(chil, indi)
      
    } else { # remove child from famg
      
      x@get_famg[[famg]]@chil_xref <- chil[-indi]

    }
    
  }
  
  x
}

refresh_indi_links <- function(x){
  
  for(famg in x@get_famg){
    
    
  }
  x
  
}

# 
# 
# for(fam in famg){
#   for(spou in c(fam@husb_xref, fam@wife_xref)){
#     
#     x@indi[[spou]] <- add_indi_family_link(x@indi[[spou]],
#                                            xref_famg = xref,
#                                            as_child = FALSE)
#   }
#   
#   if(is.null(names(chil_xref)))
#     names(chil_xref) <- rep("birth", length(chil_xref))
#   
#   for(chil in fam@chil_xref){
#     ped <- names(chil_xref[chil_xref == chil])
#     x@indi[[chil]] <- add_indi_family_link(x@indi[[chil]],
#                                            xref_famg = xref,
#                                            as_child = TRUE,
#                                            pedigree = ped)
#   }
# }
# 
# 
# 
# 

# 
# for(fam in famg){
#   for(spou in c(fam@husb_xref, fam@wife_xref)){
#     
#     x@indi[[spou]] <- add_indi_family_link(x@indi[[spou]],
#                                            xref_famg = xref,
#                                            as_child = FALSE)
#   }
#   
#   if(is.null(names(chil_xref)))
#     names(chil_xref) <- rep("birth", length(chil_xref))
#   
#   for(chil in fam@chil_xref){
#     ped <- names(chil_xref[chil_xref == chil])
#     x@indi[[chil]] <- add_indi_family_link(x@indi[[chil]],
#                                            xref_famg = xref,
#                                            as_child = TRUE,
#                                            pedigree = ped)
#   }
# }
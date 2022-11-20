
#child has additional properties that can be set (new)

# if creating a new obj from scratch, certain properties need to be preset (parent) class_new - take out presets
# if importing, all properties can be open (child)  class_general - add in properties that were preset

class_gedcomR7 <- R7::new_class("class_gedcomR7",
                            properties = list(
                              gedcom_version = R7::new_property(getter = function(self) "5.5.5"),
                              gedcom_form = R7::new_property(getter = function(self) "LINEAGE-LINKED"),
                              character_encoding = R7::new_property(getter = function(self) "UTF-8"),
                              system_id = R7::new_property(R7::class_character, default = "gedcomR7"),
                              product_name = R7::new_property(R7::class_character,
                                                          default = "The 'gedcom7' package for the R language"),
                              business_name = R7::new_property(R7::class_character, default = "Jamie Lendrum"),
                              product_version = R7::class_character,
                              business_address = R7::new_property(R7::new_union(NULL, class_address)),
                              source_data_name = R7::class_character,
                              source_data_pubdate = R7::new_property(R7::new_union(NULL, class_date_exact, R7::class_character)),
                              source_data_copyright = R7::class_character,
                              receiving_system = R7::class_character,
                              creation_date = R7::new_property(R7::new_union(NULL, class_date_exact, R7::class_character)),
                              creation_time = R7::class_character,
                              language = R7::new_property(R7::class_character, default = "English"),
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
                              
                              as_df = R7::new_property(
                                R7::class_data.frame, 
                                getter = function(self){

                                  hd <- dplyr::bind_rows(
                                    df_rows(level = 0, record = "HD", tag = "HEAD", value = ""),
                                    df_rows(level = 1, tag = "GEDC", value = ""),
                                    df_rows(level = 2, tag = "VERS", value = self@gedcom_version),
                                    df_rows(level = 2, tag = "FORM", value = self@gedcom_form),
                                    df_rows(level = 3, tag = "VERS", value = self@gedcom_version),
                                    df_rows(level = 1, tag = "CHAR", value = self@character_encoding)
                                  ) |> tidyr::fill(record)
                                  
                                  if(is.null(self@business_address)){
                                    busadd_df <- NULL
                                  } else {
                                    busadd_df <- self@business_address@as_df |> 
                                      dplyr::mutate(level = level + 2)
                                  }
                                  
                                  if(length(self@source_data_pubdate) == 0){
                                    pubdate <- character()
                                  } else if(is.character(self@source_data_pubdate)) {
                                    pubdate <- self@source_data_pubdate
                                  } else {
                                    pubdate <- self@source_data_pubdate@as_gedcom_val
                                  }
                                  
                                  if(is.null(self@creation_date)){
                                    credate <- character()
                                  } else if(is.character(self@creation_date)) {
                                    credate <- self@creation_date
                                  } else {
                                    credate <- self@creation_date@as_gedcom_val
                                  }
                                  
                                  hd_ext <- dplyr::bind_rows(
                                    df_rows(level = 0, record = "HD", tag = "SOUR", value = self@system_id),
                                    df_rows(level = 1, tag = "VERS", value = self@product_version),
                                    df_rows(level = 1, tag = "NAME", value = self@product_name),
                                    df_rows(level = 1, tag = "CORP", value = self@business_name),
                                    busadd_df,
                                    df_rows(level = 1, tag = "DATA", value = self@source_data_name),
                                    df_rows(level = 2, tag = "DATE", value = pubdate),
                                    df_rows(level = 2, tag = "COPR", value = self@source_data_copyright),
                                    df_rows(level = 0, tag = "DEST", value = self@receiving_system),
                                    df_rows(level = 0, tag = "DATE", value = credate),
                                    df_rows(level = 1, tag = "TIME", value = self@creation_time),
                                    df_rows(level = 0, tag = "LANG", value = self@language),
                                    df_rows(level = 0, tag = "SUBM", value = self@subm@xref),
                                    df_rows(level = 0, tag = "FILE", value = self@file_name),
                                    df_rows(level = 0, tag = "COPR", value = self@gedcom_copyright),
                                    df_rows(level = 0, tag = "NOTE", value = self@content_description)
                                  ) |> 
                                    tidyr::fill(record) |>
                                    dplyr::mutate(level = level + 1)
                                  
                                  # subm <- self@subm@as_df
                                  # 
                                  # linlin_rec <- c(self@indi, self@famg, self@sour,
                                  #                 self@repo, self@media, self@note) |>
                                  #   purrr::map(~ .x@as_df) |>
                                  #   dplyr::bind_rows()
                                  
                                  tr <- df_rows(record = "TR", level = 0, tag = "TRLR", value = "")
                                  
                                  dplyr::bind_rows(hd, hd_ext, tr)#subm, linlin_rec, tr)
                                  
                                }
                              )
                              
                            )
)

# CRAP - do it manually with a function
# class_gedcomR7_new <- purrr::partial(class_gedcomR7,
#                                      file_name = "test_file.ged")

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


#child has additional properties that can be set (new)

# if creating a new obj from scratch, certain properties need to be preset (parent) class_new - take out presets
# if importing, all properties can be open (child)  class_general - add in properties that were preset

class_gedcomR7 <- new_class("class_gedcomR7",
                            properties = list(
                              gedcom_version = new_property(getter = function(self) "5.5.5"),
                              gedcom_form = new_property(getter = function(self) "LINEAGE-LINKED"),
                              character_encoding = new_property(getter = function(self) "UTF-8"),
                              system_id = new_property(class_character, default = "gedcomR7"),
                              product_name = new_property(class_character,
                                                          default = "The 'gedcom7' package for the R language"),
                              business_name = new_property(class_character, default = "Jamie Lendrum"),
                              product_version = class_character,
                              business_address = new_property(new_union(NULL, class_address)),
                              source_data_name = class_character,
                              source_data_pubdate = new_property(new_union(NULL, class_date_exact)),
                              source_data_copyright = class_character,
                              receiving_system = class_character,
                              creation_date = new_property(new_union(NULL, class_date_exact)),
                              creation_time = class_character,
                              language = new_property(class_character, default = "English"),
                              xref_subm = class_character,
                              file_name = class_character,
                              gedcom_copyright = class_character,
                              content_description = class_character,
                              # Records
                              subm = class_record_subm,
                              indi = class_list,
                              famg = class_list,
                              sour = class_list,
                              repo = class_list,
                              media = class_list,
                              note = class_list,
                              
                              as_df = new_property(
                                class_data.frame, 
                                getter = function(self){

                                  hd <- dplyr::bind_rows(
                                    tibble::tibble(level = 0, record = "HD", tag = "HEAD", value = ""),
                                    tibble::tibble(level = 1, tag = "GEDC", value = ""),
                                    tibble::tibble(level = 2, tag = "VERS", value = self@gedcom_version),
                                    tibble::tibble(level = 2, tag = "FORM", value = self@gedcom_form),
                                    tibble::tibble(level = 3, tag = "VERS", value = self@gedcom_version),
                                    tibble::tibble(level = 1, tag = "CHAR", value = self@character_encoding)
                                  ) |> tidyr::fill(record)
                                  
                                  if(is.null(self@business_address)){
                                    busadd_df <- NULL
                                  } else {
                                    busadd_df <- self@business_address@as_df |> 
                                      dplyr::mutate(level = level + 2)
                                  }
                                  
                                  if(is.null(self@source_data_pubdate)){
                                    pubdate <- character()
                                  } else {
                                    pubdate <- self@source_data_pubdate@gedcom_value
                                  }
                                  
                                  if(is.null(self@creation_date)){
                                    credate <- character()
                                  } else {
                                    credate <- self@creation_date@gedcom_value
                                  }
                                  
                                  hd_ext <- dplyr::bind_rows(
                                    tibble::tibble(level = 0, record = "HD", tag = "DEST", value = self@receiving_system),
                                    tibble::tibble(level = 0, tag = "SOUR", value = self@system_id),
                                    tibble::tibble(level = 1, tag = "VERS", value = self@product_version),
                                    tibble::tibble(level = 1, tag = "NAME", value = self@product_name),
                                    tibble::tibble(level = 1, tag = "CORP", value = self@business_name),
                                    busadd_df,
                                    tibble::tibble(level = 1, tag = "DATA", value = self@source_data_name),
                                    tibble::tibble(level = 2, tag = "DATE", value = pubdate),
                                    tibble::tibble(level = 2, tag = "COPR", value = self@source_data_copyright),
                                    tibble::tibble(level = 0, tag = "DATE", value = credate),
                                    tibble::tibble(level = 1, tag = "TIME", value = self@creation_time),
                                    tibble::tibble(level = 0, tag = "LANG", value = self@language),
                                    tibble::tibble(level = 0, tag = "SUBM", value = self@subm@xref),
                                    tibble::tibble(level = 0, tag = "FILE", value = self@file_name),
                                    tibble::tibble(level = 0, tag = "COPR", value = self@gedcom_copyright),
                                    tibble::tibble(level = 0, tag = "NOTE", value = self@content_description)
                                  ) |> 
                                    tidyr::fill(record) |>
                                    dplyr::mutate(level = level + 1)
                                  
                                  # subm <- self@subm@as_df
                                  # 
                                  # linlin_rec <- c(self@indi, self@famg, self@sour,
                                  #                 self@repo, self@media, self@note) |>
                                  #   purrr::map(~ .x@as_df) |>
                                  #   dplyr::bind_rows()
                                  
                                  tr <- tibble::tibble(record = "TR", level = 0, tag = "TRLR", value = "")
                                  
                                  dplyr::bind_rows(hd, hd_ext, tr)#subm, linlin_rec, tr)
                                  
                                }
                              )
                              
                            )
)

# CRAP - do it manually with a function
# class_gedcomR7_new <- purrr::partial(class_gedcomR7,
#                                      file_name = "test_file.ged")

method(print, class_gedcomR7) <- function(x, ...){
  
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

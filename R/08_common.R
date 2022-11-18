


class_change_date <- R7::new_class("class_change_date",
                               properties = list(
                                 change_date = R7::new_property(class_date_exact, 
                                                            default = date_exact_current()),
                                 change_time = R7::class_character,
                                 notes = R7::class_list,
                                 
                                 as_df = R7::new_property(R7::class_data.frame,
                                                      getter = function(self){
                                                        
                                                        chan_df <- dplyr::bind_rows(
                                                          tibble::tibble(level = 0, tag = "CHAN", value = ""),
                                                          tibble::tibble(level = 1, tag = "DATE", value = self@change_date@as_gedcom_val),
                                                          tibble::tibble(level = 2, tag = "TIME", value = self@change_time)
                                                        )
                                                        
                                                        nts <- purrr::map(self@notes, ~ .x@as_df) |>
                                                          dplyr::bind_rows()
                                                        
                                                        if(nrow(nts) > 0) nts <- dplyr::mutate(nts, level = level + 1)
                                                        
                                                        dplyr::bind_rows(chan_df, nts)
                                                        
                                                      })
                               ),
                               validator = function(self) {
                                 c(
                                   chk_input_size(self@change_date, "@change_date", 1, 1),
                                   chk_input_size(self@change_time, "@change_time", 0, 1, 7, 12),
                                   chk_input_R7classes(self@notes, "@notes", class_note)
                                 )
                               }
)

class_note <- R7::new_class("class_note",
                   properties = list(
                     xref = R7::class_character,
                     text = R7::class_character,
                     
                     as_df = R7::new_property(R7::class_data.frame,
                                          getter = function(self){
                                           tibble::tibble(level = 0, tag = "NOTE", value = self@xref) 
                                          })
                   ),
                   validator = function(self) {
                     c(
                       chk_input_size(self@xref, "@xref", 1, 1, 3, 22),
                       chk_input_size(self@text, "@text", 1, 1, 1, 32767),
                       chk_input_pattern(self@xref, "@xref", reg_xref(TRUE))
                     )
                   }
)


class_citation <- R7::new_class("class_citation")


class_media_link <- R7::new_class("class_media_link")

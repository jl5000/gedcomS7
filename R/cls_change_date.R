
#' Create a creation date object
#' 
#' @inheritParams prop_definitions 
#' @returns An S7 object representing a GEDCOM CREATION_DATE.
#' @export
#' @tests
#' expect_error(CreationDate(date_exact = "1 JAM 2005"), regexp = "@date_exact is in an invalid format.")
#' expect_error(CreationDate(time = "123:34:45"), regexp = "@time is in an invalid format.")
#' expect_snapshot_value(CreationDate(date_exact = "1 jan 2005")@GEDCOM, "json2")
#' expect_snapshot_value(CreationDate(date_exact = "1 JAN 2005",
#'                                           time = "11:04:56")@GEDCOM, "json2")
CreationDate <- S7::new_class(
  "CreationDate",
  properties = list(
    date_exact = S7::new_property(S7::class_character | 
                                    S7::new_S3_class("gedcomS7::DateExact"),
                                  getter = function(self) self@date_exact,
                                  setter = function(self, value){
                                    if(length(value) == 0) value <- date_exact_current()
                                    if(is.character(value)) value <- toupper(value)
                                    self@date_exact <- value
                                    self
                                  },
                                  validator = function(value){
                                    c(
                                      chk_input_size(value, 1, 1),
                                      chk_input_pattern(value, reg_date_exact())
                                    )
                                  }),
    time = S7::new_property(S7::class_character | 
                              S7::new_S3_class("gedcomS7::Time"),
                            validator = function(value){
                              c(
                                chk_input_size(value, 0, 1),
                                chk_input_pattern(value, reg_time())
                              )
                            }),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          "0 CREA",
          sprintf("1 DATE %s", obj_to_val(self@date_exact)),
          sprintf("2 TIME %s", obj_to_val(self@time))
        )
      })
  )
)

#' Create a change date object
#' 
#' @inheritParams prop_definitions 
#' @returns An S7 object representing a GEDCOM CHANGE_DATE.
#' @export
#' @tests
#' expect_snapshot_value(ChangeDate(date = "1 JAN 2005",
#'                                         note_xrefs = "@23@",
#'                                         notes = c("note 1", "note 2"))@GEDCOM, "json2")
ChangeDate <- S7::new_class(
  "ChangeDate", 
  parent = CreationDate,
  properties = list(
    note_xrefs = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    chk_input_pattern(value, reg_xref(TRUE))
                                  }),
    notes = S7::new_property(S7::class_list,
                             getter = function(self) self@notes,
                             setter = function(self, value){
                               self@notes <- as.S7class_list(value, gedcomS7::Note)
                               self
                             },
                             validator = function(value){
                               for(inp in value) if(is.character(inp)) return(inp)
                             }),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          "0 CHAN",
          sprintf("1 DATE %s", obj_to_val(self@date_exact)),
          sprintf("2 TIME %s", obj_to_val(self@time)),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_xrefs)
        )
        
      })
  )
)

parse_creation_date <- function(rec_lines){
  crea_date <- find_ged_values(rec_lines, c("CREA","DATE"))
  if(length(crea_date) == 0) return(NULL)
  
  CreationDate(
    date_exact = crea_date,
    time = find_ged_values(rec_lines, c("CREA","DATE","TIME"))
  )
}

parse_change_date <- function(rec_lines){
  change_date <- find_ged_values(rec_lines, c("CHAN","DATE"))
  if(length(change_date) == 0) return(NULL)
  
  ChangeDate(
    date_exact = change_date,
    time = find_ged_values(rec_lines, c("CHAN","DATE","TIME")),
    notes = parse_notes(rec_lines, "CHAN"),
    note_xrefs = find_ged_values(rec_lines, c("CHAN","SNOTE"))
  )
}


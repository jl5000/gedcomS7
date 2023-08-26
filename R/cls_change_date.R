#' @include cls_validators.R
NULL

#' Create a creation date object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM CREATION_DATE.
#' @export
#' @include cls_date.R cls_time.R 
#' @tests
#' expect_error(class_creation_date(date_exact = "1 JAM 2005"), regexp = "@date_exact is in an invalid format.")
#' expect_error(class_creation_date(time = "123:34:45"), regexp = "@time is in an invalid format.")
#' expect_snapshot_value(class_creation_date(date_exact = "1 JAN 2005")@as_ged, "json2")
#' expect_snapshot_value(class_creation_date(date_exact = "1 JAN 2005",
#'                                           time = "11:04:56")@as_ged, "json2")
class_creation_date <- S7::new_class(
  "class_creation_date",
  package = "gedcomS7",
  properties = list(
    date_exact = S7::new_property(S7::class_character | class_date_exact, 
                            default = date_exact_current()),
    time = S7::class_character | class_time,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          "0 CREA",
          sprintf("1 DATE %s", obj_to_val(self@date_exact)),
          sprintf("2 TIME %s", obj_to_val(self@time))
        )
      })
  ),
  validator = function(self) {
    c(
      chk_input_size(self@date_exact, "@date_exact", 1, 1),
      chk_input_size(self@time, "@time", 0, 1),
      chk_input_pattern(self@date_exact, "@date_exact", reg_date_exact()),
      chk_input_pattern(self@time, "@time", reg_time())
    )
  }
)

#' Create a change date object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM CHANGE_DATE.
#' @export
#' @include cls_date.R cls_time.R cls_note.R 
#' @tests
#' expect_snapshot_value(class_change_date(date = "1 JAN 2005",
#'                                         note_xrefs = "@23@",
#'                                         notes = c("note 1", "note 2"))@as_ged, "json2")
class_change_date <- S7::new_class(
  "class_change_date", 
  package = "gedcomS7",
  parent = class_creation_date,
  properties = list(
    note_xrefs = S7::class_character,
    notes = S7::class_list | class_note | S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          "0 CHAN",
          sprintf("1 DATE %s", obj_to_val(self@date_exact)),
          sprintf("2 TIME %s", obj_to_val(self@time)),
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1)
        )
        
      })
  ),
  validator = function(self) {
    c(
      chk_input_pattern(self@note_xrefs, "@note_xrefs", reg_xref(TRUE)),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+")
    )
  }
)


#' @include cls_validators.R
NULL

#' @export
#' @include cls_date.R cls_time.R 
class_creation_date <- S7::new_class(
  "class_creation_date",
  package = "gedcomS7",
  properties = list(
    date = S7::new_property(S7::class_character | class_date_exact, 
                            default = date_exact_current()),
    time = S7::class_character | class_time,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          "0 CREA",
          sprintf("1 DATE %s", date_to_val(self@date)),
          sprintf("2 TIME %s", self@time)
        )
      })
  ),
  validator = function(self) {
    c(
      chk_input_size(self@date, "@date", 1, 1),
      chk_input_size(self@time, "@time", 0, 1),
      chk_input_pattern(self@date, "@date", reg_date_exact()),
      chk_input_pattern(self@time, "@time", reg_time())
    )
  }
)

#' @export
#' @include cls_note.R 
class_change_date <- S7::new_class(
  "class_change_date", 
  package = "gedcomS7",
  parent = class_creation_date,
  properties = list(
    note_uids = S7::class_character,
    notes = S7::class_list | class_note | S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          "0 CHAN",
          sprintf("1 DATE %s", date_to_val(self@date)),
          sprintf("2 TIME %s", self@time),
          sprintf("1 SNOTE %s", self@note_uids),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1)
        )
        
      })
  ),
  validator = function(self) {
    c(
      chk_input_pattern(self@note_uids, "@note_uids", reg_uuid(TRUE)),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+")
    )
  }
)


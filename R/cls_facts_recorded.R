#' @include cls_validators.R
NULL

#' Create an object recording facts covered in a source record
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM SOUR.EVEN structure.
#' @export
#' @include cls_date.R cls_place.R
#' @tests
#' expect_error(class_facts_recorded("birth"), regexp = "@fact_types is in an invalid format")
#' expect_error(class_facts_recorded("BIRT "), regexp = "@fact_types is in an invalid format")
#' expect_error(class_facts_recorded("BIRT,DEAT"), regexp = "@fact_types is in an invalid format")
#' expect_error(class_facts_recorded("BIRT, DEAT", date_period = "2006"), 
#'                                   regexp = "@date_period is in an invalid format")
#' expect_snapshot_value(class_facts_recorded("BIRT, DEAT")@as_ged, "json2")
#' expect_snapshot_value(class_facts_recorded("BIRT, DEAT",
#'                                            date_period = "FROM 2007 TO 2010")@as_ged, "json2")
#' expect_snapshot_value(class_facts_recorded("BIRT, DEAT",
#'                                            date_period = "FROM 2007 TO 2010",
#'                                            date_phrase = "sometime",
#'                                            territory = "somewhere")@as_ged, "json2")
class_facts_recorded <- S7::new_class(
  "class_facts_recorded",
  package = "gedcomS7",
  properties = list(
    fact_types = S7::class_character,
    date_period = S7::class_character | class_date_period,
    date_phrase = S7::class_character,
    territory = S7::class_character | class_place,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 EVEN %s", self@fact_types),
          sprintf("1 DATE %s", obj_to_val(self@date_period)) |> trimws(),
          sprintf("2 PHRASE %s", self@date_phrase),
          obj_to_ged(self@territory, "PLAC") |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@fact_types, "@fact_types", 1, 1),
      chk_input_pattern(self@fact_types, "@fact_types", sprintf("^(%s)(, (%s))*$", 
                                                                paste(val_fact_types(), collapse = "|"),
                                                                paste(val_fact_types(), collapse = "|"))),
      chk_input_size(self@date_period, "@date_period", 0, 1),
      chk_input_pattern(self@date_period, "@date_period", reg_date_period()),
      chk_input_size(self@date_phrase, "@date_phrase", 0, 1, 1),
      chk_input_size(self@territory, "@territory", 0, 1),
      chk_input_pattern(self@territory, "@territory", ".+")
    )
  })

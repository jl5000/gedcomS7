#' @include cls_validators.R
NULL

#' @export
#' @include cls_date.R cls_place.R
class_events_recorded <- S7::new_class(
  "class_events_recorded",
  package = "gedcomS7",
  properties = list(
    events = S7::class_character,
    date_period = S7::class_character | class_date_period,
    date_phrase = S7::class_character,
    jurisdiction_place = S7::class_character | class_place,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 EVEN %s", self@events),
          sprintf("1 DATE %s", date_to_val(self@date_period)),
          sprintf("2 PHRASE %s", self@date_phrase),
          obj_to_ged(self@jurisdiction_place, "PLAC") |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@events, "@events", 1, 1),
      chk_input_size(self@date_period, "@date_period", 0, 1),
      chk_input_pattern(self@date_period, "@date_period", reg_date_period()),
      chk_input_size(self@date_phrase, "@date_phrase", 0, 1, 1),
      chk_input_size(self@jurisdiction_place, "@jurisdiction_place", 0, 1, 1)
    )
  })

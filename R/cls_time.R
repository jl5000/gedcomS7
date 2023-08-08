#' @include cls_validators.R
NULL

#' @export
class_time <- S7::new_class(
  "class_time",
  package = "gedcomS7",
  properties = list(
    hour = S7::class_numeric,
    minute = S7::class_numeric,
    second = S7::class_numeric,
    fraction = S7::class_numeric,
    utc = S7::new_property(S7::class_logical, default = TRUE),
    
    as_val = S7::new_property(
      S7::class_character,
      getter = function(self){
        tim <- sprintf("%02d:%02d", self@hour, self@minute)
        if(length(self@second) == 1)
          tim <- sprintf("%s:%02d", tim, self@second)
        if(length(self@fraction) == 1)
          tim <- paste0(tim, ".", self@fraction)
        if(self@utc)
          tim <- paste0(tim, "Z")
        tim
      }
    )
  ),
  validator = function(self){
    c(
      chk_whole_number(self@hour, "@hour"),
      chk_whole_number(self@minute, "@minute"),
      chk_whole_number(self@second, "@second"),
      chk_whole_number(self@fraction, "@fraction"),
      chk_input_size(self@hour, "@hour", 1, 1, 0, 23),
      chk_input_size(self@minute, "@minute", 1, 1, 0, 59),
      chk_input_size(self@second, "@second", 0, 1, 0, 59),
      chk_input_size(self@fraction, "@fraction", 0, length(self@second), 1),
      chk_input_size(self@utc, "@utc", 1, 1)
    )
  })
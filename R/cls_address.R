#' @include cls_validators.R
NULL

#' Create an address object
#' 
#' @inheritParams prop_definitions
#' @return An S7 object representing a GEDCOM ADDRESS_STRUCTURE.
#' @export
#' @tests
#' expect_error(class_address(), "@full has too few elements")
#' expect_error(class_address(""), "@full has too few characters")
#' expect_snapshot_value(class_address("street\ncity\nstate")@as_ged, "json2")
#' expect_snapshot_value(class_address("street\ncity\nstate",
#'                                     city = "this city")@as_ged, "json2")
#' expect_snapshot_value(class_address("street\ncity\nstate",
#'                                     state = "this state")@as_ged, "json2")
#' expect_snapshot_value(class_address("street\ncity\nstate",
#'                                     country = "this country")@as_ged, "json2")
#' expect_snapshot_value(class_address("street\ncity\nstate",
#'                                     city = "this city",
#'                                     state = "this state",
#'                                     country = "this country")@as_ged, "json2")
#' expect_snapshot_value(class_address("street\ncity\nstate",
#'                                     city = "this city",
#'                                     state = "this state",
#'                                     country = "this country",
#'                                     postal_code = "81309")@as_ged, "json2")
class_address <- S7::new_class(
  "class_address",
  package = "gedcomS7",
  properties = list(
    full = S7::class_character,
    city = S7::class_character,
    state = S7::class_character,
    postal_code = S7::class_character,
    country = S7::class_character,
    
    as_val = S7::new_property(S7::class_character, 
                              getter = function(self) self@full),
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 ADDR %s", self@full),
          sprintf("1 CITY %s", self@city),
          sprintf("1 STAE %s", self@state),
          sprintf("1 POST %s", self@postal_code),
          sprintf("1 CTRY %s", self@country)
        )
      })
  ),
  
  validator = function(self) {
    c(
      chk_input_size(self@full, "@full", 1, 1, 1),
      chk_input_size(self@city, "@city", 0, 1, 1),
      chk_input_size(self@state, "@state", 0, 1, 1),
      chk_input_size(self@postal_code, "@postal_code", 0, 1, 1),
      chk_input_size(self@country, "@country", 0, 1, 1)
    )
  }
)


extract_address <- function(lines, location = NULL){
  
  addr <- find_ged_values(lines, c(location, "ADDR"))
  if(length(addr) == 0) return(character())
  
  class_address(
    full = addr,
    city = find_ged_values(lines, c(location, "ADDR","CITY")),
    state = find_ged_values(lines, c(location, "ADDR","STAE")),
    postal_code = find_ged_values(lines, c(location, "ADDR","POST")),
    country = find_ged_values(lines, c(location, "ADDR","CTRY"))
  )
}
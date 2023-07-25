#' @include cls_validators.R
NULL

#' Create an address object
#' 
#' @param full A full address as it would appear on a mailing label, with lines separated
#' by \n. For example:
#' "The White House\n1600 Pennsylvania Avenue, N.W.\nWashington, DC 20500\nUnited States of America"
#' @param city Optional. The city component of the address.
#' @param state Optional. The state component of the address.
#' @param postal_code Optional. The postal code component of the address.
#' @param country Optional. The country component of the address.
#' 
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

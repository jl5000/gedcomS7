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
    full = S7::new_property(S7::class_character,
                            validator = function(value){
                              chk_input_size(value, 1, 1, 1)
                            }),
    city = S7::new_property(S7::class_character,
                            validator = function(value){
                              chk_input_size(value, 0, 1, 1)
                            }),
    state = S7::new_property(S7::class_character,
                             validator = function(value){
                               chk_input_size(value, 0, 1, 1)
                             }),
    postal_code = S7::new_property(S7::class_character,
                                   validator = function(value){
                                     chk_input_size(value, 0, 1, 1)
                                   }),
    country = S7::new_property(S7::class_character,
                               validator = function(value){
                                 chk_input_size(value, 0, 1, 1)
                               }),
    adr1 = S7::new_property(S7::class_character,
                               validator = function(value){
                                 chk_input_size(value, 0, 1, 1)
                               }),
    adr2 = S7::new_property(S7::class_character,
                               validator = function(value){
                                 chk_input_size(value, 0, 1, 1)
                               }),
    adr3 = S7::new_property(S7::class_character,
                               validator = function(value){
                                 chk_input_size(value, 0, 1, 1)
                               }),
    
    as_val = S7::new_property(S7::class_character, 
                              getter = function(self) self@full),
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 ADDR %s", gsub("; ", "\n", self@full)),
          sprintf("1 ADR1 %s", self@adr1),
          sprintf("1 ADR2 %s", self@adr2),
          sprintf("1 ADR3 %s", self@adr3),
          sprintf("1 CITY %s", self@city),
          sprintf("1 STAE %s", self@state),
          sprintf("1 POST %s", self@postal_code),
          sprintf("1 CTRY %s", self@country)
        )
      })
  )
)


extract_address <- function(lines, location = NULL){
  
  addr <- find_ged_values(lines, c(location, "ADDR"))
  if(length(addr) == 0) return(character())
  
  class_address(
    full = gsub("\n", "; ", addr),
    adr1 = find_ged_values(lines, c(location, "ADDR","ADR1")),
    adr2 = find_ged_values(lines, c(location, "ADDR","ADR2")),
    adr3 = find_ged_values(lines, c(location, "ADDR","ADR3")),
    city = find_ged_values(lines, c(location, "ADDR","CITY")),
    state = find_ged_values(lines, c(location, "ADDR","STAE")),
    postal_code = find_ged_values(lines, c(location, "ADDR","POST")),
    country = find_ged_values(lines, c(location, "ADDR","CTRY"))
  )
}

#' Create an address object
#' 
#' @param full A full address as it would appear on a mailing label, with lines separated
#' by semi-colon and a space. For example:
#' "The White House; 1600 Pennsylvania Avenue N.W.; Washington, DC 20500; United States of America"
#' @param adr1 Deprecated.
#' @param adr2 Deprecated.
#' @param adr3 Deprecated.
#' @param city The city component of the address.
#' @param state The state component of the address.
#' @param postal_code The postal code component of the address.
#' @param country The country component of the address.
#' 
#' @returns An S7 object representing a GEDCOM ADDRESS_STRUCTURE.
#' @export
#' @tests
#' expect_error(Address(), "@full has too few elements")
#' expect_error(Address(""), "@full has too few characters")
#' expect_snapshot_value(Address("street\ncity\nstate")@GEDCOM, "json2")
#' expect_snapshot_value(Address("street\ncity\nstate",
#'                                     city = "this city")@GEDCOM, "json2")
#' expect_snapshot_value(Address("street\ncity\nstate",
#'                                     state = "this state")@GEDCOM, "json2")
#' expect_snapshot_value(Address("street\ncity\nstate",
#'                                     country = "this country")@GEDCOM, "json2")
#' expect_snapshot_value(Address("street\ncity\nstate",
#'                                     city = "this city",
#'                                     state = "this state",
#'                                     country = "this country")@GEDCOM, "json2")
#' expect_snapshot_value(Address("street\ncity\nstate",
#'                                     city = "this city",
#'                                     state = "this state",
#'                                     country = "this country",
#'                                     postal_code = "81309")@GEDCOM, "json2")
Address <- S7::new_class(
  "Address",
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
    
    GEDCOM_STRING = S7::new_property(S7::class_character, 
                              getter = function(self) self@full),
    
    GEDCOM = S7::new_property(
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


parse_address <- function(lines, location = NULL){
  
  addr <- find_ged_values(lines, c(location, "ADDR"))
  if(length(addr) == 0) return(NULL)
  
  Address(
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

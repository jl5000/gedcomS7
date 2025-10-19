
# Must appear first before other classes
GedcomS7class <- S7::new_class("GedcomS7class", abstract = TRUE)

S7::method(print, GedcomS7class) <- function(x, ...){
  summary(x)
}

void_xref <- function() "@VOID@"
new_xref <- function() "@GEDCOMS7_ORPHAN@"

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
  parent = GedcomS7class,
  properties = list(
    full = prop_char(1, 1, 1),
    city = prop_char(0, 1, 1),
    state = prop_char(0, 1, 1),
    postal_code = prop_char(0, 1, 1),
    country = prop_char(0, 1, 1),
    adr1 = prop_char(0, 1, 1),
    adr2 = prop_char(0, 1, 1),
    adr3 = prop_char(0, 1, 1),
    
    GEDCOM_STRING = S7::new_property(S7::class_character, 
                              getter = function(self) self@full),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          as_ged(gsub("; ", "\n", self@full), "ADDR", 0),
          as_ged(self@adr1, "ADR1", 1),
          as_ged(self@adr2, "ADR2", 1),
          as_ged(self@adr3, "ADR3", 1),
          as_ged(self@city, "CITY", 1),
          as_ged(self@state, "STAE", 1),
          as_ged(self@postal_code, "POST", 1),
          as_ged(self@country, "CTRY", 1)
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


S7::method(summary, Address) <- function(object, ...){
  to_console("Address:", object@GEDCOM_STRING, 15)
}

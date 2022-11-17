library(R7)
source("patterns.R")
source("validators.R")
source("dates.R")
source("locations.R")
source("links.R")
source("facts.R")
source("personal_name.R")
source("common.R")
source("record.R")
source("gedcom.R")


x = class_gedcomR7(product_version = "1.1",
                   business_address = class_address(city = "cit", state = "sta", postal_code = "PO11 8DG",
                                                    country = "England", phone_numbers = c("2342423","342342","23234"),
                                                    emails = "aja@dssd.com", faxes = c("212321","2342332")),
                   source_data_name = "sour", source_data_pubdate = class_date_exact(1L,3L,2001L),
                   source_data_copyright = "copy", receiving_system = "random", 
                   creation_date = class_date_exact(5L, 9L, 1900L), xref_subm = "@U1@", file_name = "dd.ged",
                   gedcom_copyright = "gecopy", content_description = "desc")


x@indi[[1]] = class_record_indi()
x@indi[[2]] = class_record_indi()
x




# 
# 
# 
# 
# # GEDCOM - INDI - EVENT STRUCTURE - EVENT DETAIL - CITATION - NOTE
# 
# 
# 
# myged@indi("@I123")@event("birth")[1]@citation[5]@notes[1:2]
# 
# myged |>
#   get_indi("@123@") |>
#   get_event("birth", 1) |>
#   get_citation(5) |>
#   get_notes(1:2)
# 
# 
# # METHODS
# # add_notes, get_notes, remove_notes
# 
# add_note <- new_generic("add_note", "x", function(x, xref) {
#   R7_dispatch()
# })
# 
# method(add_note, name_pieces) <- function(x, xref){
#   x@notes[[length(x@notes) + 1]] <- notes(xref = xref)
#   x
# }
# 
# 
# # add_note: citation, place structure, name pieces, event_detail, child to family link,
#  # association structure, records except note
# 
# 
# 
# 
# x <- name_pieces(given = "Jamie", surname = "Lendrum")
# x <- name_pieces(surname = "Lendrum")
# x <- name_pieces(suffix = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
# x@surname <- "Lendru"

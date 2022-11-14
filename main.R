
library(R7)

source("gedcom.R")
source("record.R")
source("subrecords.R")
source("helpers.R")

# GEDCOM - INDI - EVENT STRUCTURE - EVENT DETAIL - CITATION - NOTE



myged@indi("@I123")@event("birth")[1]@citation[5]@notes[1:2]

myged |>
  get_indi("@123@") |>
  get_event("birth", 1) |>
  get_citation(5) |>
  get_notes(1:2)


# METHODS
# add_notes, get_notes, remove_notes

add_note <- new_generic("add_note", "x", function(x, xref) {
  R7_dispatch()
})

method(add_note, name_pieces) <- function(x, xref){
  x@notes[[length(x@notes) + 1]] <- notes(xref = xref)
  x
}


# add_note: citation, place structure, name pieces, event_detail, child to family link,
 # association structure, records except note




x <- name_pieces(given = "Jamie", surname = "Lendrum")
x <- name_pieces(surname = "Lendrum")
x <- name_pieces(suffix = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
x@surname <- "Lendru"

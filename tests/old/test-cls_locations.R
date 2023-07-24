
test_that("class_place", {
  expect_error(class_place())
  expect_error(class_place(""))
  expect_snapshot_value(class_place("here")@as_ged, "json2")
  expect_error(class_place("here", lat_long = "123 543"))
  expect_snapshot_value(class_place("here", lat_long = "N12 E56")@as_ged, "json2")
  expect_error(class_place("here", phon_names = "hare"))
  expect_snapshot_value(class_place("here", phon_names = c(phon1 = "hare", phon2 = "hire"))@as_ged, "json2")
  expect_snapshot_value(class_place("here", 
                                    phon_names = c(phon1 = "hare", phon2 = "hire"),
                                    rom_names = c(romn1 = "haare", romn2 = "hiire"),
                                    lat_long = "N12 E56")@as_ged, "json2")
  expect_snapshot_value(class_place("here", 
                                    phon_names = c(phon1 = "hare", phon2 = "hire"),
                                    rom_names = c(romn1 = "haare", romn2 = "hiire"),
                                    lat_long = "N12 E56",
                                    note_links = c("@N1@","@N562@"),
                                    notes = "Thing 1")@as_ged, "json2")
})




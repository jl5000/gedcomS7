
test_that("class_place", {
  expect_error(class_place())
  expect_error(class_place(""))
  expect_snapshot_value(class_place("here")@as_df, "serialize")
  expect_error(class_place("here", lat_long = "123 543"))
  expect_snapshot_value(class_place("here", lat_long = "N12 E56")@as_df, "serialize")
  expect_error(class_place("here", phon_names = "hare"))
  expect_snapshot_value(class_place("here", phon_names = c(phon1 = "hare", phon2 = "hire"))@as_df, "serialize")
  expect_snapshot_value(class_place("here", 
                                    phon_names = c(phon1 = "hare", phon2 = "hire"),
                                    rom_names = c(romn1 = "haare", romn2 = "hiire"),
                                    lat_long = "N12 E56")@as_df, "serialize")
  expect_snapshot_value(class_place("here", 
                                    phon_names = c(phon1 = "hare", phon2 = "hire"),
                                    rom_names = c(romn1 = "haare", romn2 = "hiire"),
                                    lat_long = "N12 E56",
                                    note_links = c("@N1@","@N562@"),
                                    notes = "Thing 1")@as_df, "serialize")
})


test_that("class_address", {
  expect_null(class_address()@as_df)
  expect_snapshot_value(class_address("street")@as_df, "serialize")
  expect_snapshot_value(class_address(c("street","village"))@as_df, "serialize")
  expect_snapshot_value(class_address(c("street","village"),
                                      city = "this city")@as_df, "serialize")
  expect_snapshot_value(class_address(c("street","village"),
                                      state = "this state")@as_df, "serialize")
  expect_snapshot_value(class_address(c("street","village"),
                                      country = "this country")@as_df, "serialize")
  expect_snapshot_value(class_address(c("street","village"),
                                      city = "this city",
                                      state = "this state",
                                      country = "this country")@as_df, "serialize")
  expect_snapshot_value(class_address(c("street","village"),
                                      city = "this city",
                                      phone_numbers = c("123445","6788990"))@as_df, "serialize")
})

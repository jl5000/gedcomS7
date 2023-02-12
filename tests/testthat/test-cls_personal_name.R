
test_that("class_name_info", {
  expect_error(class_name_info())
  expect_error(class_name_info(full = "Joe /Bloggs/"))
  expect_snapshot_value(class_name_info(full = "Joe /Bloggs/",
                                        given = "Joe",
                                        surname = "Bloggs",
                                        notes = "Birth name")@as_ged, "json2")
})

test_that("class_personal_name", {
  expect_error(class_personal_name())
  expect_snapshot_value(class_personal_name(
    name = class_name_info(full = "Joe /Bloggs/",
                           type = "birth",
                           given = "Joe",
                           surname = "Bloggs"),
    phon_names = list(class_name_info(full = "Joe /Blox/",
                                      type = "variation",
                                      given = "Joe",
                                      surname = "Blox")),
    rom_names = list(class_name_info(full = "Joe /Blogs/",
                                     type = "spelling",
                                     given = "Joe",
                                     surname = "Blogs")))@as_ged, "json2")
  expect_error(class_personal_name(
    name = class_name_info(full = "Joe /Bloggs/",
                           type = "birth",
                           given = "Joe",
                           surname = "Bloggs"),
    phon_names = list(class_name_info(full = "Joe /Blox/",
                                      given = "Joe",
                                      surname = "Blox"))))
  
})

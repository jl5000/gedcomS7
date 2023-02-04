
test_that("class_change_date", {
  expect_error(class_change_date(date = "1 JAM 2005"))
  expect_error(class_change_date(time = "123:34:45"))
  expect_snapshot_value(class_change_date(date = "1 JAN 2005")@as_ged, "json2")
  expect_snapshot_value(class_change_date(date = "1 JAN 2005",
                                          time = "11:04:56")@as_ged, "json2")
  expect_snapshot_value(class_change_date(date = "1 JAN 2005",
                                          notes = c("note 1", "note 2"))@as_ged, "json2")
})

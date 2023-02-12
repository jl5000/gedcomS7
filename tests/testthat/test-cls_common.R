
test_that("class_change_date", {
  expect_error(class_change_date(date = "1 JAM 2005"), regex = "@date is in an invalid format.")
  expect_error(class_change_date(time = "123:34:45"), regex = "@time is in an invalid format.")
  expect_snapshot_value(class_change_date(date = "1 JAN 2005")@as_ged, "json2")
  expect_snapshot_value(class_change_date(date = "1 JAN 2005",
                                          time = "11:04:56")@as_ged, "json2")
  expect_snapshot_value(class_change_date(date = "1 JAN 2005",
                                          notes = c("note 1", "note 2"))@as_ged, "json2")
})

test_that("class_citation", {
  expect_error(class_citation(), regexp = "@xref has too few dimensions")
  expect_error(class_citation(xref = "ref"), regexp = "@xref is in an invalid format")
  expect_snapshot_value(class_citation(xref = "@S1@")@as_ged, "json2")
  
  expect_snapshot_value(class_citation(xref = "@S1@",
                                       where = "Page 1",
                                       event_type = "BIRT",
                                       event_role = "FATH",
                                       recording_date = "JAN 1994",
                                       source_text = c("This is","source text"),
                                       media_links = "@O1@",
                                       note_links = c("@N1@","@N3@"),
                                       notes = "This is a note",
                                       certainty = "2")@as_ged, "json2")
})
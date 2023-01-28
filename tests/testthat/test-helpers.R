
test_that("delete_ged_section", {
  ged <- readLines(system.file("extdata", "MINIMAL555.GED", package = "gedcomR7"))
  
  expect_snapshot_value(delete_ged_section(ged, 2), "json")
  expect_snapshot_value(delete_ged_section(ged, 4), "json")
  expect_snapshot_value(delete_ged_section(ged, 6), "json")
  expect_snapshot_value(delete_ged_section(ged, 10), "json")
})


test_that("Extract line components", {
  ged <- readLines(system.file("extdata", "555SAMPLE.GED", package = "gedcomS7"))
  
  expect_snapshot_value(extract_ged_level(ged), "json2")
  expect_snapshot_value(extract_ged_xref(ged), "json2")
  expect_snapshot_value(extract_ged_tag(ged), "json2")
  expect_snapshot_value(extract_ged_value(ged), "json2")
})

test_that("delete_ged_section", {
  ged <- readLines(system.file("extdata", "MINIMAL555.GED", package = "gedcomS7"))
  
  expect_snapshot_value(delete_ged_section(ged, 2), "json2")
  expect_snapshot_value(delete_ged_section(ged, 4), "json2")
  expect_snapshot_value(delete_ged_section(ged, 6), "json2")
  expect_snapshot_value(delete_ged_section(ged, 10), "json2")
})

test_that("Extraction functions", {
  # record
  lines <- readLines(system.file("extdata", "555SAMPLE.GED", package = "gedcomS7"))[28:46]
  lines <- c(lines,
             "1 REFN 123",
             "1 CHAN",
             "2 DATE 23 APR 2001",
             "3 TIME 12:24:45",
             "2 NOTE @N1@",
             "2 NOTE Note for change date",
             "1 REFN 456",
             "2 TYPE refn2",
             "1 REFN 789")
  
  expect_equal(find_ged_values(lines, "NAME"), "Robert Eugene /Williams/")
  expect_equal(find_ged_values(lines, c("NAME","SURN")), "Williams")
  expect_equal(find_ged_values(lines, c("BIRT","SOUR","PAGE")), "Sec. 2, p. 45")
  expect_equal(extract_vals_and_types(lines, "REFN"), c("123", refn2 = "456", "789"))
  expect_snapshot_value(obj_to_ged(extract_change_date(lines)), "json2")
  expect_snapshot_value(lst_to_ged(extract_facts_indi(lines)), "json2")
  expect_snapshot_value(lst_to_ged(extract_personal_names(lines)), "json2")
})

test_that("Increase level", {
  ged <- readLines(system.file("extdata", "555SAMPLE.GED", package = "gedcomS7"))
  
  expect_snapshot_value(increase_level(ged, by = 2), "json2")
})

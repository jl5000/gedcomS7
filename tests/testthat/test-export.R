
test_that("Create CONC lines", {
  test1 <- c(
    "0 TEMP",
    "1 TAG A very very long line of values which need to be split."
  )
  test2 <- c(
    "0 TEMP This line value is really long but this time it's on the first line",
    "1 TAG A tag"
  )
  test3 <- c(
    "0 TEMP",
    "1 TAG value",
    "2 FROM Another value but this time it's not the last line nor the first line",
    "1 ADDR Address"
  )
  
  expect_snapshot_value(create_conc_lines(test1, 6), "json2")
  expect_snapshot_value(create_conc_lines(test2, 7), "json2")
  expect_snapshot_value(create_conc_lines(test3, 9), "json2")
})


test_that("Create CONT lines", {
  test1 <- c(
    "0 TEMP",
    "1 TAG This is a line\nthen this\nand this\nalso this"
  )
  test2 <- c(
    "0 TEMP",
    "1 TAG A tag",
    "2 QUAY This is a line\nthen this\nand this\nalso this",
    "1 DATE Today"
  )
  
  expect_snapshot_value(create_cont_lines(test1), "json2")
  expect_snapshot_value(create_cont_lines(test2), "json2")
})

test_that("Split gedcom values", {
  test <- c(
    as.character(1:6),
    "0 STAR This line ends here\nNew one here",
    "1 TEMP This line is a really really long one that needs concatenation by also\nstarts on a new line here.",
    "1 BGDS OK",
    "2 LINE Another line\nwith multiple split\nbut also long enough to need concatenation."
  )
  
  expect_snapshot_value(split_gedcom_values(test, 4), "json2")
  
  expect_equal(combine_gedcom_values(split_gedcom_values(test, 4)), test)
})


test_that("Export gedcom", {
  expect_error(write_gedcom(read_gedcom(system.file("extdata", "555SAMPLE.GED", package = "gedcomR7")), 
                            "my_family.txt"), regexp = "Output is not being saved as a GEDCOM file")
  
  expect_identical(
    read_gedcom(system.file("extdata", "555SAMPLE.GED", package = "gedcomR7")),
    read_gedcom(system.file("extdata", "555SAMPLE.GED", package = "gedcomR7")) |> 
      write_gedcom("555Sample.ged") |> 
      read_gedcom()
  )
  file.remove("555Sample.ged")
})

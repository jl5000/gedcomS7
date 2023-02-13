
test_that("Import gedcom", {
  expect_error(read_gedcom("my_family.txt"), regexp = "GEDCOM file should have a .ged extension")
  
  expect_snapshot_value(
    read_gedcom(system.file("extdata", "555SAMPLE.GED", package = "gedcomS7"))@as_ged, 
    "json2")
  expect_snapshot_value(
    read_gedcom(system.file("extdata", "555SAMPLE16BE.GED", package = "gedcomS7"))@as_ged, 
    "json2")
  expect_snapshot_value(
    read_gedcom(system.file("extdata", "555SAMPLE16LE.GED", package = "gedcomS7"))@as_ged, 
    "json2")
  expect_snapshot_value(
    read_gedcom(system.file("extdata", "MINIMAL555.GED", package = "gedcomS7"))@as_ged, 
    "json2")
})

test_that("Read GEDCOM encoding", {
  expect_equal(
    read_gedcom_encoding(system.file("extdata", "555SAMPLE.GED", package = "gedcomS7")),
    "UTF-8"
  )
  expect_equal(
    read_gedcom_encoding(system.file("extdata", "555SAMPLE16BE.GED", package = "gedcomS7")),
    "UTF-16BE"
  )
  expect_equal(
    read_gedcom_encoding(system.file("extdata", "555SAMPLE16LE.GED", package = "gedcomS7")),
    "UTF-16LE"
  )
})

test_that("Validate lines", {
  
  ged <- readLines(system.file("extdata", "555SAMPLE.GED", package = "gedcomS7"))
  
  gedd <- ged # a copy
  gedd[20] <- paste(rep("a", .pkgenv$gedcom_line_length_limit + 1), collapse = "")
  expect_error(validate_lines(gedd), regexp = "The following lines are too long: 20")
  
  gedd <- ged
  gedd[13] <- paste("3 WWW", paste(rep("a", .pkgenv$gedcom_phys_value_limit + 1), collapse = ""))
  expect_error(validate_lines(gedd), regexp = "The following lines have values which are too long: 13")
  
  gedd <- ged
  #gedd[21] <- "1 ADDR "
  gedd[22] <- "23 ADR1 1900 43rd Street West"
  gedd[23] <- "2 CITYBillings"
  gedd[24] <- " 2 STAE Montana"
  expect_error(validate_lines(gedd), regexp = "The following lines are invalid:\n22:[^\n]*\n23:[^\n]*\n24:")
  
  gedd <- ged
  gedd[68] <- "2 DATE @#DFRENCH R@ 16 Mar 1864"
  expect_error(validate_lines(gedd), regexp = "Non-Gregorian calendar dates are not supported.")
  
  expect_null(validate_lines(ged))
})


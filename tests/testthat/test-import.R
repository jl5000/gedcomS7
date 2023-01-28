
test_that("Read GEDCOM encoding", {
  expect_equal(
    read_gedcom_encoding(system.file("extdata", "555SAMPLE.GED", package = "gedcomR7")),
    "UTF-8"
  )
  expect_equal(
    read_gedcom_encoding(system.file("extdata", "555SAMPLE16BE.GED", package = "gedcomR7")),
    "UTF-16BE"
  )
  expect_equal(
    read_gedcom_encoding(system.file("extdata", "555SAMPLE16LE.GED", package = "gedcomR7")),
    "UTF-16LE"
  )
})

test_that("Validate lines", {
  
  ged <- readLines(system.file("extdata", "555SAMPLE.GED", package = "gedcomR7"))
  
  gedd <- ged # a copy
  gedd[20] <- paste(rep("a", .pkgenv$gedcom_line_length_limit + 1), collapse = "")
  expect_error(validate_lines(gedd), regexp = "The following lines are too long: 20")
  
  gedd <- ged
  gedd[13] <- paste("3 WWW", paste(rep("a", .pkgenv$gedcom_phys_value_limit + 1), collapse = ""))
  expect_error(validate_lines(gedd), regexp = "The following lines have values which are too long: 13")
  
  gedd <- ged
  gedd[21] <- "1 ADDR "
  gedd[22] <- "23 ADR1 1900 43rd Street West"
  gedd[23] <- "2 CITYBillings"
  gedd[24] <- " 2 STAE Montana"
  expect_error(validate_lines(gedd), regexp = "The following lines are invalid:\n21:[^\n]*\n22:[^\n]*\n23:[^\n]*\n24:")
  
  gedd <- ged
  gedd[68] <- "2 DATE @#DFRENCH R@ 16 Mar 1864"
  expect_error(validate_lines(gedd), regexp = "Non-Gregorian calendar dates are not supported.")
  
  expect_null(validate_lines(ged))
})


test_that("Extraction functions", {
  # record
  lines <- c(
    "0 @I123@ INDI",
    "1 SEX M",
    "1 CHAN",
    "2 DATE 23 APR 2001",
    "3 TIME 12:24:45",
    "2 NOTE @N1@",
    "2 NOTE Note for change date",
    "1 NAME Joe /Bloggs/",
    "2 GIVN Joe",
    "2 SURN Bloggs",
    "2 NOTE This is a\nnew line",
    "1 REFN 123",
    "1 SOUR @S1@",
    "2 PAGE Page 4",
    "2 EVEN BIRT",
    "3 ROLE FATH",
    "2 DATA",
    "3 DATE JAN 1984",
    "3 TEXT source text",
    "2 OBJE @O2@",
    "2 OBJE @O1@",
    "2 NOTE Citation note",
    "2 NOTE @N1@",
    "2 QUAY 3",
    "1 SOUR @S2@",
    "1 REFN 456",
    "2 TYPE refn2",
    "1 REFN 789"
  )
  
  expect_equal(find_ged_values(lines, return_xref = TRUE), "@I123@")
  expect_equal(find_ged_values(lines, "NAME"), "Joe /Bloggs/")
  expect_equal(find_ged_values(lines, c("NAME","SURN")), "Bloggs")
  expect_equal(find_ged_values(lines, c("NAME","NOTE")), "This is a\nnew line")
  expect_equal(extract_refns(lines), c("123", refn2 = "456", "789"))
  expect_snapshot_value(extract_change_date(lines), "serialize")
  expect_snapshot_value(extract_citations(lines), "serialize")
})

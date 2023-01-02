
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
  
  expect_equal(extract_ged_values(lines, return_xref = TRUE), "@I123@")
  expect_equal(extract_ged_values(lines, "NAME"), "Joe /Bloggs/")
  expect_equal(extract_ged_values(lines, c("NAME","SURN")), "Bloggs")
  expect_equal(extract_refns(lines), c("123", refn2 = "456", "789"))
  expect_snapshot_value(extract_change_date(lines), "serialize")
  expect_snapshot_value(extract_citations(lines), "serialize")
})

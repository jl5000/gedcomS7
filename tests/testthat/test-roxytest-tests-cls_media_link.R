# Generated by roxytest: do not edit by hand!

# File R/cls_media_link.R: @tests

test_that("Function MediaLink() @ L29", {
  expect_snapshot_value(MediaLink()@GEDCOM, "json2")
  expect_error(MediaLink("@O4"), regexp = "@media_xref is in an invalid format")
  expect_snapshot_value(MediaLink("@1@")@GEDCOM, "json2")
  expect_snapshot_value(MediaLink("@1@", 
                                         title = "new title")@GEDCOM, "json2")
  expect_snapshot_value(MediaLink("@1@", 
                                         title = "new title",
                                         top = 5, left = 200)@GEDCOM, "json2")
})


# Generated by roxytest: do not edit by hand!

# File R/cls_translation.R: @tests

test_that("Function TranslationText() @ L15", {
  expect_error(TranslationText(), regexp = "@text has too few elements")
  expect_error(TranslationText(letters[1:2]), regexp = "@text has too many elements")
  expect_snapshot_value(TranslationText("test", language = "en")@GEDCOM, "json2")
})


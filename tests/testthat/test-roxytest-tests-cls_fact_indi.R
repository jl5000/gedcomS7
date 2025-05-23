# Generated by roxytest: do not edit by hand!

# File R/cls_fact_indi.R: @tests

test_that("Function IndividualEvent() @ L63", {
  expect_error(IndividualEvent("birth", fact_val = "Y"), 
               regexp = "This is not a valid @fact_type for this event")
  expect_error(IndividualEvent("BIRT", fact_val = "Yes"), 
               regexp = "Only a @fact_val of 'Y' is permitted for this event")
  expect_error(IndividualEvent("DEAT", fact_val = "Y", fam_xref = "@12@"), 
               regexp = "Only adoption, birth, and christening events can have a @fam_xref")
  expect_error(IndividualEvent("BIRT", fact_val = "Y", fam_xref = "@12@", adop_parent = "HUSB"), 
               regexp = "Only adoption events can have a @adop_parent or @adop_parent_phrase")
  expect_error(IndividualEvent("EVEN", fact_desc = "Fact desc"), 
               regexp = "A @fact_val is required for this fact")
  expect_error(IndividualEvent("ADOP", fact_val = "Y", fam_xref = "@12@", adop_parent = "man"), 
               regexp = "@adop_parent has an invalid value")
  expect_error(IndividualEvent("ADOP", fact_val = "Y", adop_parent = "BOTH"), 
               regexp = "@adop_parent requires a @fam_xref")
  expect_error(IndividualEvent("ADOP", fact_val = "Y", fam_xref = "@12@", adop_parent_phrase = "both of them"), 
               regexp = "@adop_parent_phrase requires a @adop_parent")
  expect_error(IndividualEvent("BIRT", unique_ids = "ABC"), regexp = "@unique_ids is in an invalid format")
  expect_snapshot_value(IndividualEvent("BIRT", fact_val = "Y")@GEDCOM, "json2")
  expect_error(IndividualEvent("DEAT", age = "73"), regexp = "@age is in an invalid format")
  expect_snapshot_value(IndividualEvent("DEAT", fact_val = "Y")@GEDCOM, "json2")
  expect_snapshot_value(IndividualEvent("DEAT", fact_val = "Y", age_phrase = "old")@GEDCOM, "json2")
  expect_snapshot_value(IndividualEvent("DEAT", fact_val = "Y", age = "73y 4m",
                                        age_phrase = "old")@GEDCOM, "json2")
  expect_snapshot_value(IndividualEvent("ADOP", fact_val = "Y",
                                         date = "jan 1980",
                                         fact_desc = "More info on adoption",
                                         fam_xref = "@123@",
                                         adop_parent = "WIFE",
                                         adop_parent_phrase = "Gloria")@GEDCOM, "json2")
})


test_that("Function IndividualAttribute() @ L158", {
  expect_error(IndividualAttribute("descr", fact_val = "Tall"), 
               regexp = "This is not a valid @fact_type for this attribute")
  expect_error(IndividualAttribute("DSCR"), 
               regexp = "A @fact_val is required for this fact")
  expect_error(IndividualAttribute("NCHI", fact_val = "2.4"), 
               regexp = "Number of children/marriages must be a whole number")
  expect_snapshot_value(IndividualAttribute("NCHI", 3)@GEDCOM, "json2")
  expect_snapshot_value(IndividualAttribute("FACT", "Diabetes",
                                   fact_desc = "Medical condition",
                                   date = "26 JUN 2001",
                                   place = Place("here",
                                                       notes = "place note"),
                                   address = "street, town, city, country",
                                   phone_numbers = "123455",
                                   emails = "things@domain.com",
                                   web_pages = "www.domain.com",
                                   cause = "Chocolate",
                                   locked = TRUE,
                                   date_sort = "2008",
                                   associations = Association("@I45@", relation_is = "GODP"),
                                   notes = "another note",
                                   note_xrefs = "@N45@",
                                   citations = "@S67@",
                                   unique_ids = "7ddf39aa-42a8-4995-94eb-4392bcc00d28")@GEDCOM, 
                         "json2")
})


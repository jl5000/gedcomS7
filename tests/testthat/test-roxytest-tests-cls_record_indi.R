# Generated by roxytest: do not edit by hand!

# File R/cls_record_indi.R: @tests

test_that("Function IndividualRecord() @ L53", {
  expect_warning(IndividualRecord(pers_names = "Me"),
                 regexp = "Did you forget to enclose the surname in forward slashes")
  expect_warning(IndividualRecord(pers_names = list(PersonalName("Joe /Bloggs/"), "Me")),
                 regexp = "Did you forget to enclose the surname in forward slashes")
  nms <- list(PersonalName("Joe /Bloggs/"),
              PersonalName("Joseph /Bloggs/"))
  fcts <- list(IndividualEvent("BIRT", date = "2005", place = "USA"),
               IndividualEvent("BIRT", date = "2006", place = "Colorado, USA"),
               IndividualEvent("DEAT", date = "18 JUN 2020", place = "London, UK"),
               IndividualEvent("DEAT", date = "2021", place = "UK"))
  expect_equal(IndividualRecord(pers_names = nms)@PRIMARY_NAME, "Joe Bloggs")
  expect_equal(IndividualRecord(pers_names = nms)@ALL_NAMES, c("Joe Bloggs","Joseph Bloggs"))
  birt_deat <- IndividualRecord(facts = fcts)
  expect_equal(birt_deat@BIRTH_DATE, "2005")
  expect_equal(birt_deat@BIRTH_PLACE, "USA")
  expect_equal(birt_deat@DEATH_DATE, "18 JUN 2020")
  expect_equal(birt_deat@DEATH_PLACE, "London, UK")
  expect_snapshot_value(IndividualRecord("@I4@", sex = "M", facts = fcts, pers_names = nms,
                                          fam_links_chil = "@F132@", 
                                          fam_links_spou = "@F67@")@GEDCOM, "json2")
})


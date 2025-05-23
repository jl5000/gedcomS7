# Generated by roxytest: do not edit by hand!

# File R/cls_address.R: @tests

test_that("Function Address() @ L44", {
  expect_error(Address(), "@full has too few elements")
  expect_error(Address(""), "@full has too few characters")
  expect_snapshot_value(Address("street\ncity\nstate")@GEDCOM, "json2")
  expect_snapshot_value(Address("street\ncity\nstate",
                                      city = "this city")@GEDCOM, "json2")
  expect_snapshot_value(Address("street\ncity\nstate",
                                      state = "this state")@GEDCOM, "json2")
  expect_snapshot_value(Address("street\ncity\nstate",
                                      country = "this country")@GEDCOM, "json2")
  expect_snapshot_value(Address("street\ncity\nstate",
                                      city = "this city",
                                      state = "this state",
                                      country = "this country")@GEDCOM, "json2")
  expect_snapshot_value(Address("street\ncity\nstate",
                                      city = "this city",
                                      state = "this state",
                                      country = "this country",
                                      postal_code = "81309")@GEDCOM, "json2")
})


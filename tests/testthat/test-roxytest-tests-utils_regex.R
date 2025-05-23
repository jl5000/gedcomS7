# Generated by roxytest: do not edit by hand!

# File R/utils_regex.R: @tests

test_that("Function reg_time() @ L74", {
  expect_equal(grepl(reg_time(TRUE), "0:00"), TRUE)
  expect_equal(grepl(reg_time(TRUE), "23:59"), TRUE)
  expect_equal(grepl(reg_time(TRUE), "0:00:00"), TRUE)
  expect_equal(grepl(reg_time(TRUE), "23:59:59"), TRUE)
  expect_equal(grepl(reg_time(TRUE), "0:00:00.000"), TRUE)
  expect_equal(grepl(reg_time(TRUE), "23:59:59.9999"), TRUE)
  expect_equal(grepl(reg_time(TRUE), "00:00"), TRUE)
  expect_equal(grepl(reg_time(TRUE), "24:59"), FALSE)
  expect_equal(grepl(reg_time(TRUE), "23:60"), FALSE)
  expect_equal(grepl(reg_time(TRUE), "23:59:60"), FALSE)
  expect_equal(grepl(reg_time(TRUE), "23:59:59."), FALSE)
  expect_equal(grepl(reg_time(TRUE), "23:59:59:a"), FALSE)
})


test_that("Function reg_date_exact() @ L155", {
  expect_equal(grepl(reg_date_exact(), "14 JAN 2005"), TRUE)
  expect_equal(grepl(reg_date_exact(), "14 JAM 2005"), FALSE)
  expect_equal(grepl(reg_date_exact(strict = FALSE), "32 JAN 2005"), TRUE)
  expect_equal(grepl(reg_date_exact(), "JAN 2005"), FALSE)
  expect_equal(grepl(reg_date_exact(), "14 JAN 2005/06"), FALSE)
  expect_equal(grepl(reg_date_exact(), "5 JUL 2005"), TRUE)
  expect_equal(grepl(reg_date_exact(strict = FALSE), "35 JUL 2005"), TRUE)
  expect_equal(grepl(reg_date_exact(), "8 NOV 1956/57"), FALSE)
  expect_equal(grepl(reg_date_exact(), "2005"), FALSE)
  expect_equal(grepl(reg_date_exact(), "15 NOV 125"), TRUE)
  expect_equal(grepl(reg_date_exact(), "JAN 1901/58"), FALSE)
  expect_equal(grepl(reg_date_exact(), "5 JUL 2005 "), FALSE)
  expect_equal(grepl(reg_date_exact(), " 5 JUL 2005"), FALSE)
})


test_that("Function reg_date() @ L187", {
  expect_equal(grepl(reg_date(), "14 JAN 2005"), TRUE)
  expect_equal(grepl(reg_date(), "14 JAM 2005"), FALSE)
  expect_equal(grepl(reg_date(), "JAN 2005"), TRUE)
  expect_equal(grepl(reg_date(), "5 JUL 2005"), TRUE)
  expect_equal(grepl(reg_date(), "8 NOV 1956/57"), FALSE)
  expect_equal(grepl(reg_date(), "2005"), TRUE)
  expect_equal(grepl(reg_date(), "15 NOV 125"), TRUE)
  expect_equal(grepl(reg_date(), "5 JUL 2005 "), FALSE)
  expect_equal(grepl(reg_date(), " 5 JUL 2005"), FALSE)
})


test_that("Function reg_date_period() @ L227", {
  expect_equal(grepl(reg_date_period(), ""), TRUE)
  expect_equal(grepl(reg_date_period(), "FROM 14 JAN 2005"), TRUE)
  expect_equal(grepl(reg_date_period(), "TO 14 JAM 2005"), FALSE)
  expect_equal(grepl(reg_date_period(), "FROM JAN 2005"), TRUE)
  expect_equal(grepl(reg_date_period(), "FROM 14 JAN 2005/06 TO 2007"), FALSE)
  expect_equal(grepl(reg_date_period(), "TO 5 JUL 2005"), TRUE)
  expect_equal(grepl(reg_date_period(), "TO  8 NOV 1956"), FALSE)
  expect_equal(grepl(reg_date_period(), "FROM 2005"), TRUE)
  expect_equal(grepl(reg_date_period(), "FROM 15 NOV 125"), TRUE)
  expect_equal(grepl(reg_date_period(), " TO JAN 1901"), FALSE)
  expect_equal(grepl(reg_date_period(), "FROM 5 JUL 2005 "), FALSE)
  expect_equal(grepl(reg_date_period(), " TO 5 JUL 2005"), FALSE)
})


test_that("Function reg_date_range() @ L262", {
  expect_equal(grepl(reg_date_range(), "BEF 14 JAN 2005"), TRUE)
  expect_equal(grepl(reg_date_range(), "AFT 14 JAM 2005"), FALSE)
  expect_equal(grepl(reg_date_range(), "BEF JAN 2005"), TRUE)
  expect_equal(grepl(reg_date_range(), "BET 14 JAN 2005/06 AND 2007"), FALSE)
  expect_equal(grepl(reg_date_range(), "AFT 5 JUL 2005"), TRUE)
  expect_equal(grepl(reg_date_range(), "AFT  8 NOV 1956"), FALSE)
  expect_equal(grepl(reg_date_range(), "BEF 2005"), TRUE)
  expect_equal(grepl(reg_date_range(), "BEF 15 NOV 125"), TRUE)
  expect_equal(grepl(reg_date_range(), " AFT JAN 1901"), FALSE)
  expect_equal(grepl(reg_date_range(), "BEF 5 JUL 2005 "), FALSE)
  expect_equal(grepl(reg_date_range(), " AFT 5 JUL 2005"), FALSE)
})


test_that("Function reg_date_approximated() @ L296", {
  expect_equal(grepl(reg_date_approximated(), "ABT 14 JAN 2005"), TRUE)
  expect_equal(grepl(reg_date_approximated(), "CAL 14 JAM 2005"), FALSE)
  expect_equal(grepl(reg_date_approximated(), "EST JAN 2005"), TRUE)
  expect_equal(grepl(reg_date_approximated(), "ABT 14 JAN 2005 AND 2007"), FALSE)
  expect_equal(grepl(reg_date_approximated(), "EST 5 JUL 2005"), TRUE)
  expect_equal(grepl(reg_date_approximated(), "CAL  8 NOV 1956"), FALSE)
  expect_equal(grepl(reg_date_approximated(), "ABT 2005"), TRUE)
  expect_equal(grepl(reg_date_approximated(), "CAL 15 NOV 125"), TRUE)
  expect_equal(grepl(reg_date_approximated(), " EST JAN 1901"), FALSE)
  expect_equal(grepl(reg_date_approximated(), "CAL 5 JUL 2005 "), FALSE)
  expect_equal(grepl(reg_date_approximated(), " CAL 5 JUL 2005"), FALSE)
})


test_that("Function reg_date_value() @ L338", {
  expect_equal(grepl(reg_date_value(), "14 JAN 2005"), TRUE)
  expect_equal(grepl(reg_date_value(), "MAR 1901"), TRUE)
  expect_equal(grepl(reg_date_value(), "2010"), TRUE)
  expect_equal(grepl(reg_date_value(), "FROM 14 FEB 2005"), TRUE)
  expect_equal(grepl(reg_date_value(), "TO JAN 2005"), TRUE)
  expect_equal(grepl(reg_date_value(), "FROM 14 JAN 2005/06 TO 2007"), FALSE)
  expect_equal(grepl(reg_date_value(), "BEF 5 JUL 2005"), TRUE)
  expect_equal(grepl(reg_date_value(), "AFT 8 NOV 1956/57"), FALSE)
  expect_equal(grepl(reg_date_value(), "BET 2005 AND MAR 2008"), TRUE)
  expect_equal(grepl(reg_date_value(), "CAL 15 NOV 1925"), TRUE)
  expect_equal(grepl(reg_date_value(), "EST JAN 1901/58"), FALSE)
  expect_equal(grepl(reg_date_value(), "ABT 5 JUL 2005"), TRUE)
  expect_equal(grepl(reg_date_value(), "14 JAN 205"), TRUE)
  expect_equal(grepl(reg_date_value(), "MAR 1901 "), FALSE)
  expect_equal(grepl(reg_date_value(), " 2010"), FALSE)
  expect_equal(grepl(reg_date_value(), "FROM 14 FEBR 2005"), FALSE)
  expect_equal(grepl(reg_date_value(), "TO  JAN 2005"), FALSE)
  expect_equal(grepl(reg_date_value(), "FROM 14 JAN 2005 AND 2007"), FALSE)
  expect_equal(grepl(reg_date_value(), "BEF 5 JUL 2005 "), FALSE)
  expect_equal(grepl(reg_date_value(), "AFT 8 NOV 1956/1957"), FALSE)
  expect_equal(grepl(reg_date_value(), "BET 2005 TO MAR 2008"), FALSE)
  expect_equal(grepl(reg_date_value(), "CAL 15 NOV 1925/"), FALSE)
  expect_equal(grepl(reg_date_value(), "14TH JAN 1901"), FALSE)
  expect_equal(grepl(reg_date_value(), "ABT 5  JUL 2005"), FALSE)
})


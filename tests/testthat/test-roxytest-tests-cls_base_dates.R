# Generated by roxytest: do not edit by hand!

# File R/cls_base_dates.R: @tests

test_that("Function DateExact() @ L19", {
  expect_error(DateExact(), regexp = "@year has too few.*@month has too few.*@day has too few")
  expect_error(DateExact(2001), regexp = "@month has too few.*@day has too few")
  expect_error(DateExact(2001, 5), regexp = "@day has too few")
  expect_error(DateExact(2001, 5, 32), regexp = "@day has a value which is too high")
  expect_error(DateExact(2001, 2, 29), regexp = "Invalid date")
  expect_equal(DateExact(2001, 5, 2)@GEDCOM_STRING, "2 MAY 2001")
  expect_equal(DateExact(28, 7, 12)@GEDCOM_STRING, "12 JUL 28")
  expect_equal(DateExact(28, 7, 12)@as_date, as.Date("28-07-12"))
})


test_that("Function date_exact_current() @ L71", {
  expect_equal(date_exact_current()@as_date, Sys.Date())
})


test_that("Function DateGregorian() @ L101", {
  expect_error(DateGregorian(), regexp = "@year has too few elements")
  expect_error(DateGregorian(2001, day = 15), regexp = "Day is defined without a month")
  expect_error(DateGregorian(day = 5), regexp = "@year has too few elements")
  expect_error(DateGregorian(month = 10), regexp = "@year has too few elements")
  expect_error(DateGregorian(2010, 13, 3), regexp = "@month has a value which is too high")
  expect_error(DateGregorian(2010, 1, 32), regexp = "@day has a value which is too high")
  expect_error(DateGregorian(320, 5, 16, bce = TRUE), regexp = "BCE date must contain year only")
  expect_equal(DateGregorian(2001, 5, 12)@GEDCOM_STRING, "12 MAY 2001")
  expect_equal(DateGregorian(2004, 2, 29)@GEDCOM_STRING, "29 FEB 2004")
  expect_equal(DateGregorian(2004, 8)@GEDCOM_STRING, "AUG 2004")
  expect_equal(DateGregorian(2012)@GEDCOM_STRING, "2012")
  expect_equal(DateGregorian(193, bce = TRUE)@GEDCOM_STRING, "193 BCE")
})


test_that("Function DateApprox() @ L171", {
  expect_error(DateApprox("hello"), regexp = "@date_greg is in an invalid format")
  expect_equal(DateApprox(DateGregorian(2001, 5, 12), calc = TRUE)@GEDCOM_STRING, 
                                "CAL 12 MAY 2001")
  expect_equal(DateApprox(DateGregorian(2004, 2, 29), about = TRUE)@GEDCOM_STRING, 
                                "ABT 29 FEB 2004")
  expect_equal(DateApprox(DateGregorian(2004, 8), est = TRUE)@GEDCOM_STRING, 
                                 "EST AUG 2004")
})


test_that("Function DatePeriod() @ L262", {
  expect_equal(DatePeriod()@GEDCOM_STRING, "")
  expect_error(DatePeriod(""), regexp = "@start_date is in an invalid format")
  expect_error(DatePeriod(end_date = ""), regexp = "@end_date is in an invalid format")
  expect_equal(DatePeriod("2 jul 1989")@GEDCOM_STRING, "FROM 2 JUL 1989")
  expect_equal(DatePeriod(end_date = "2 Jul 1989")@GEDCOM_STRING, "TO 2 JUL 1989")
  expect_equal(
    DatePeriod(
      start_date = DateGregorian(1995, 6, 1)
    )@GEDCOM_STRING, "FROM 1 JUN 1995")
  expect_equal(
    DatePeriod(
      end_date = DateGregorian(1995, 6, 1)
    )@GEDCOM_STRING, "TO 1 JUN 1995")
  expect_equal(
    DatePeriod(
      start_date = DateGregorian(1990, 6, 1),
      end_date = DateGregorian(1995, 3)
    )@GEDCOM_STRING, "FROM 1 JUN 1990 TO MAR 1995")
  expect_error(
    DatePeriod(
      start_date = DateGregorian(1995, 6, 1),
      end_date = DateGregorian(1995, 6, 1)
    ), regexp = "Start date is the same as end date")
  expect_error(
    DatePeriod(
      start_date = DateGregorian(2005, 6, 1),
      end_date = DateGregorian(1995, 6, 1)
    ), regexp = "Start date comes after end date")
  expect_error(
    DatePeriod(
      start_date = DateGregorian(2005, 8, 1),
      end_date = DateGregorian(2005, 6, 1)
    ), regexp = "Start date comes after end date")
  expect_error(
    DatePeriod(
      start_date = DateGregorian(2005, 8, 10),
      end_date = DateGregorian(2005, 8, 1)
    ), regexp = "Start date comes after end date")
})


test_that("Function DateRange() @ L360", {
  expect_error(DateRange(), regexp = "has too few elements")
  expect_error(DateRange(""), regexp = "@start_date is in an invalid format")
  expect_error(DateRange(end_date = ""), regexp = "@end_date is in an invalid format")
  expect_equal(DateRange("2 JUL 1989")@GEDCOM_STRING, "AFT 2 JUL 1989")
  expect_equal(DateRange(end_date = "2 JUL 1989")@GEDCOM_STRING, "BEF 2 JUL 1989")
  expect_equal(
    DateRange(
      start_date = DateGregorian(1995, 6, 1)
    )@GEDCOM_STRING, "AFT 1 JUN 1995")
  expect_equal(
    DateRange(
      end_date = DateGregorian(1995, 6, 1)
    )@GEDCOM_STRING, "BEF 1 JUN 1995")
  expect_equal(
    DateRange(
      start_date = DateGregorian(1990, 6, 1),
      end_date = DateGregorian(1995, 3)
    )@GEDCOM_STRING, "BET 1 JUN 1990 AND MAR 1995")
  expect_error(
    DateRange(
     start_date = DateGregorian(1995, 6, 1),
      end_date = DateGregorian(1995, 6, 1)
    ), regexp = "Start date is the same as end date")
  expect_error(
    DateRange(
      start_date = DateGregorian(2005, 6, 1),
      end_date = DateGregorian(1995, 6, 1)
    ), regexp = "Start date comes after end date")
  expect_error(
    DateRange(
      start_date = DateGregorian(2005, 8, 1),
      end_date = DateGregorian(2005, 6, 1)
    ), regexp = "Start date comes after end date")
  expect_error(
    DateRange(
      start_date = DateGregorian(2005, 8, 10),
      end_date = DateGregorian(2005, 8, 1)
    ), regexp = "Start date comes after end date")
})


test_that("Function DateValue() @ L399", {
  expect_error(DateValue("FROM 2016", time = "12:34"), regexp = "A date period should not have a time defined")
  expect_error(DateValue(DatePeriod(end_date = "1980"), time = Time(3,45,54,6765)), 
               regexp = "A date period should not have a time defined")
  expect_error(DateValue(""), regexp = "A @date_phrase must be given if @date is ''")
  expect_equal(DateValue(DateGregorian(2005, 1, 5))@GEDCOM_STRING, "5 JAN 2005")
  expect_snapshot_value(DateValue("aft 1990", date_phrase = "Maybe 1992")@GEDCOM, "json2")
  expect_snapshot_value(DateValue("", date_phrase = "Phrase only", time = "02:24")@GEDCOM, "json2")
})


test_that("Function DateSorting() @ L473", {
  expect_error(DateSorting(""), regexp = "@date is in an invalid format")
  expect_error(DateSorting("FROM 2016"), regexp = "@date is in an invalid format")
  expect_error(DateSorting(DatePeriod(end_date = "1980")), 
               regexp = "@date must be <character> or ")
  expect_equal(DateSorting(DateGregorian(2005, 1, 5))@GEDCOM_STRING, "5 JAN 2005")
  expect_snapshot_value(DateSorting("1990", date_phrase = "Maybe 1992")@GEDCOM, "json2")
})


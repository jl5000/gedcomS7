
test_that("class_time", {
  expect_error(class_time(), regexp = "@hour has too few dimensions.+@minute has too few dimensions")
  expect_error(class_time(3), regexp = "@minute has too few dimensions")
  expect_equal(class_time(5, 6)@as_val, "05:06Z")
  expect_equal(class_time(5, 59, 19)@as_val, "05:59:19Z")
  expect_equal(class_time(0, 0, 0, 6)@as_val, "00:00:00.6Z")
  expect_equal(class_time(14, 28, utc = FALSE)@as_val, "14:28")
})

test_that("class_date_exact", {
  expect_error(class_date_exact(), regexp = "@day.*@month.*@year")
  expect_error(class_date_exact(2001), regexp = "@day.*@month")
  expect_error(class_date_exact(2001, 5), regexp = "@day")
  expect_error(class_date_exact(2001, 5, 32), regexp = "Invalid date")
  expect_equal(class_date_exact(2001, 5, 2)@as_val, "2 MAY 2001")
  expect_equal(class_date_exact(2001, 5, 12)@as_date, as.Date("2001-05-12"))
  expect_equal(date_exact_current()@as_date, Sys.Date())
})

test_that("class_date_calendar", {
  expect_error(class_date_calendar(), regexp = "@year has too few dimensions")
  expect_error(class_date_calendar(2001, day = 15), regexp = "Day is defined without a month")
  expect_error(class_date_calendar(day = 5), regexp = "@year has too few dimensions")
  expect_error(class_date_calendar(month = 10), regexp = "@year has too few dimensions")
  expect_error(class_date_calendar(2010, 13, 3), regexp = "@month has a value which is too high")
  expect_error(class_date_calendar(2010, 1, 32), regexp = "@day has a value which is too high")
  expect_error(class_date_calendar(320, 5, 16, bce = TRUE), regexp = "BCE date must contain year only")
  expect_equal(class_date_calendar(2001, 5, 12)@as_val, "12 MAY 2001")
  expect_equal(class_date_calendar(2004, 2, 29)@as_val, "29 FEB 2004")
  expect_equal(class_date_calendar(2004, 8)@as_val, "AUG 2004")
  expect_equal(class_date_calendar(2012)@as_val, "2012")
  expect_equal(class_date_calendar(193, bce = TRUE)@as_val, "193 BCE")
})

test_that("class_date_period", {
  expect_error(class_date_period(), regexp = "@start_date \\+ @end_date has too few dimensions")
  expect_equal(
    class_date_period(
      start_date = class_date_calendar(1995, 6, 1)
    )@as_val, "FROM 1 JUN 1995")
  expect_equal(
    class_date_period(
      end_date = class_date_calendar(1995, 6, 1)
    )@as_val, "TO 1 JUN 1995")
  expect_equal(
    class_date_period(
      start_date = class_date_calendar(1990, 6, 1),
      end_date = class_date_calendar(1995, 3)
    )@as_val, "FROM 1 JUN 1990 TO MAR 1995")
  expect_error(
    class_date_period(
      start_date = class_date_calendar(1995, 6, 1),
      end_date = class_date_calendar(1995, 6, 1)
    ), regexp = "Start date is the same as end date")
  expect_error(
    class_date_period(
      start_date = class_date_calendar(2005, 6, 1),
      end_date = class_date_calendar(1995, 6, 1)
    ), regexp = "Start date comes after end date")
  expect_error(
    class_date_period(
      start_date = class_date_calendar(2005, 8, 1),
      end_date = class_date_calendar(2005, 6, 1)
    ), regexp = "Start date comes after end date")
  expect_error(
    class_date_period(
      start_date = class_date_calendar(2005, 8, 10),
      end_date = class_date_calendar(2005, 8, 1)
    ), regexp = "Start date comes after end date")
})

test_that("class_date_range", {
  expect_error(class_date_range(), regexp = "@start_date \\+ @end_date has too few dimensions")
  expect_equal(
    class_date_range(
      start_date = class_date_calendar(1995, 6, 1)
    )@as_val, "AFT 1 JUN 1995")
  expect_equal(
    class_date_range(
      end_date = class_date_calendar(1995, 6, 1)
    )@as_val, "BEF 1 JUN 1995")
  expect_equal(
    class_date_range(
      start_date = class_date_calendar(1990, 6, 1),
      end_date = class_date_calendar(1995, 3)
    )@as_val, "BET 1 JUN 1990 AND MAR 1995")
  expect_error(
    class_date_range(
      start_date = class_date_calendar(1995, 6, 1),
      end_date = class_date_calendar(1995, 6, 1)
    ), regexp = "Start date is the same as end date")
  expect_error(
    class_date_range(
      start_date = class_date_calendar(2005, 6, 1),
      end_date = class_date_calendar(1995, 6, 1)
    ), regexp = "Start date comes after end date")
  expect_error(
    class_date_range(
      start_date = class_date_calendar(2005, 8, 1),
      end_date = class_date_calendar(2005, 6, 1)
    ), regexp = "Start date comes after end date")
  expect_error(
    class_date_range(
      start_date = class_date_calendar(2005, 8, 10),
      end_date = class_date_calendar(2005, 8, 1)
    ), regexp = "Start date comes after end date")
})

test_that("class_date_approx", {
  expect_equal(class_date_approx(class_date_calendar(2001, 5, 12),
                                 calc = TRUE)@as_val, "CAL 12 MAY 2001")
  expect_equal(class_date_approx(class_date_calendar(2004, 2, 29),
                                 about = TRUE)@as_val, "ABT 29 FEB 2004")
  expect_equal(class_date_approx(class_date_calendar(2004, 8),
                                 est = TRUE)@as_val, "EST AUG 2004")
})

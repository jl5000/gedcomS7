

test_that("class_date_exact", {
  expect_error(class_date_exact())
  expect_error(class_date_exact(2001L))
  expect_error(class_date_exact(2001L, 5L))
  expect_error(class_date_exact(2001L, 5L, 12))
  expect_error(class_date_exact(2001L, 5L, 32L), regexp = "Invalid date")
  expect_equal(class_date_exact(2001L, 5L, 12L)@as_val, "12 MAY 2001")
  expect_equal(class_date_exact(2001L, 5L, 12L)@as_date, as.Date("2001-05-12"))
  expect_equal(date_exact_current()@as_date, Sys.Date())
})

test_that("class_date_calendar", {
  expect_error(class_date_calendar())
  expect_error(class_date_calendar(2001L, day = 15L))
  expect_error(class_date_calendar(day = 5L))
  expect_error(class_date_calendar(month = 10L))
  expect_error(class_date_calendar(2010L, 13L, 3L))
  expect_error(class_date_calendar(2010L, 1L, 32L))
  expect_error(class_date_calendar(month = 1L, day = 3L, year_is_bce = TRUE))
  expect_equal(class_date_calendar(2001L, 5L, 12L)@as_val, "12 MAY 2001")
  expect_equal(class_date_calendar(2004L, 2L, 29L)@as_val, "29 FEB 2004")
  expect_equal(class_date_calendar(2004L, 8L)@as_val, "AUG 2004")
  expect_equal(class_date_calendar(2012L)@as_val, "2012")
  expect_equal(class_date_calendar(month = 3L, day = 25L)@as_val, "25 MAR")
  expect_equal(class_date_calendar(193L, year_is_bce = TRUE)@as_val, "193 BCE")
})

test_that("class_date_period", {
  expect_error(class_date_period())
  expect_equal(
    class_date_period(
      start_date = class_date_calendar(1995L, 6L, 1L)
    )@as_val, "FROM 1 JUN 1995")
  expect_equal(
    class_date_period(
      end_date = class_date_calendar(1995L, 6L, 1L)
    )@as_val, "TO 1 JUN 1995")
  expect_equal(
    class_date_period(
      start_date = class_date_calendar(1990L, 6L, 1L),
      end_date = class_date_calendar(1995L, 3L)
    )@as_val, "FROM 1 JUN 1990 TO MAR 1995")
  expect_error(
    class_date_period(
      start_date = class_date_calendar(1995L, 6L, 1L),
      end_date = class_date_calendar(1995L, 6L, 1L)
    ))
  expect_error(
    class_date_period(
      start_date = class_date_calendar(2005L, 6L, 1L),
      end_date = class_date_calendar(1995L, 6L, 1L)
    ))
  expect_error(
    class_date_period(
      start_date = class_date_calendar(2005L, 8L, 1L),
      end_date = class_date_calendar(2005L, 6L, 1L)
    ))
  expect_error(
    class_date_period(
      start_date = class_date_calendar(2005L, 8L, 10L),
      end_date = class_date_calendar(2005L, 8L, 1L)
    ))
})

test_that("class_date_range", {
  expect_error(class_date_range())
  expect_equal(
    class_date_range(
      start_date = class_date_calendar(1995L, 6L, 1L)
    )@as_val, "AFT 1 JUN 1995")
  expect_equal(
    class_date_range(
      end_date = class_date_calendar(1995L, 6L, 1L)
    )@as_val, "BEF 1 JUN 1995")
  expect_equal(
    class_date_range(
      start_date = class_date_calendar(1990L, 6L, 1L),
      end_date = class_date_calendar(1995L, 3L)
    )@as_val, "BET 1 JUN 1990 AND MAR 1995")
  expect_error(
    class_date_range(
      start_date = class_date_calendar(1995L, 6L, 1L),
      end_date = class_date_calendar(1995L, 6L, 1L)
    ))
  expect_error(
    class_date_range(
      start_date = class_date_calendar(2005L, 6L, 1L),
      end_date = class_date_calendar(1995L, 6L, 1L)
    ))
  expect_error(
    class_date_range(
      start_date = class_date_calendar(2005L, 8L, 1L),
      end_date = class_date_calendar(2005L, 6L, 1L)
    ))
  expect_error(
    class_date_range(
      start_date = class_date_calendar(2005L, 8L, 10L),
      end_date = class_date_calendar(2005L, 8L, 1L)
    ))
})

test_that("class_date_approx", {
  expect_equal(class_date_approx(class_date_calendar(2001L, 5L, 12L),
                                 calc = TRUE)@as_val, "CAL 12 MAY 2001")
  expect_equal(class_date_approx(class_date_calendar(2004L, 2L, 29L),
                                 about = TRUE)@as_val, "ABT 29 FEB 2004")
  expect_equal(class_date_approx(class_date_calendar(2004L, 8L),
                                 est = TRUE)@as_val, "EST AUG 2004")
})

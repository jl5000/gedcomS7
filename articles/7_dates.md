# Date types

## Introduction

As you would expect with a genealogical data format, dates form a major
part of information contained within it. Within `gedcomS7` there are a
number of different date types that can be defined, and there is a
helper function for each type to ensure it is formatted correctly.

## Exact dates

Dates that occur on a specific defined date are defined using
[`DateExact()`](https://jl5000.github.io/gedcomS7/reference/DateExact.md):

``` r
library(gedcomS7)

DateExact(1999, 7, 5)
#> Date:     5 JUL 1999
DateExact(1956, 12, 8)
#> Date:     8 DEC 1956
DateExact(2008, 4, 1)
#> Date:     1 APR 2008
```

As well as the GEDCOM formatted strings (printed above and also accessed
through the `@GEDCOM_STRING` property), this date object can also be
expressed as a date type:

``` r
DateExact(2008, 4, 1)@as_date
#> [1] "2008-04-01"
```

## Calendar dates

Calendar dates are the most common type of date used. They can not only
be used to create specific dates like
[`DateExact()`](https://jl5000.github.io/gedcomS7/reference/DateExact.md),
they underpin the other date types described below. They can be created
using
[`DateCalendar()`](https://jl5000.github.io/gedcomS7/reference/DateCalendar.md):

``` r
DateCalendar(year = 1999, month = 4, day = 5)
#> Date:     5 APR 1999
DateCalendar(year = 1999, month = 4)
#> Date:     APR 1999
DateCalendar(year = 1999)
#> Date:     1999
```

Years before the Common Era can be defined using negative years:

``` r
DateCalendar(year = -105)
#> Date:     105 BCE
```

The Gregorian calendar is implicitly assumed by default. The Julian
calendar can be specified by setting `julian = TRUE`:

``` r
DateCalendar(year = 1999, month = 4, julian = TRUE)
#> Date:     JULIAN APR 1999
```

No other calendars apart from the Gregorian and Julian calendars are
supported by the package.

## Approximate dates

Approximate dates (i.e. those expressing uncertainty) use the qualifiers
‘about’, ‘calculated’, or ‘estimated’ in conjunction with a
[`DateCalendar()`](https://jl5000.github.io/gedcomS7/reference/DateCalendar.md)
object:

``` r
DateCalendar(year = 1999, month = 4, day = 5) |> DateApprox(about = TRUE)
#> Date:     ABT 5 APR 1999
DateCalendar(year = 1999, month = 4, day = 5) |> DateApprox(calc = TRUE)
#> Date:     CAL 5 APR 1999
DateCalendar(year = 1999, month = 4, day = 5) |> DateApprox(est = TRUE)
#> Date:     EST 5 APR 1999
```

## Date periods

Date periods can take one or two
[`DateCalendar()`](https://jl5000.github.io/gedcomS7/reference/DateCalendar.md)
objects:

``` r
DatePeriod(start_date = DateCalendar(1956, 7, 26),
           end_date = DateCalendar(1956, 9, 15))
#> Date:     FROM 26 JUL 1956 TO 15 SEP 1956
DatePeriod(start_date = DateCalendar(1956, 7, 26))
#> Date:     FROM 26 JUL 1956
DatePeriod(end_date = DateCalendar(1956, 9, 15))
#> Date:     TO 15 SEP 1956
```

Providing only one date gives a semi-infinite period. If a mix of
calendars are used with a finite period, they are made explicit, e.g.

``` r
DatePeriod(start_date = DateCalendar(1956, 7, 26, julian = TRUE),
           end_date = DateCalendar(1956, 9, 15))
#> Date:     FROM JULIAN 26 JUL 1956 TO GREGORIAN 15 SEP 1956
```

## Date ranges

Date ranges can be defined using
[`DateRange()`](https://jl5000.github.io/gedcomS7/reference/DateRange.md).
Unlike
[`DatePeriod()`](https://jl5000.github.io/gedcomS7/reference/DatePeriod.md),
they describe when an event occurred rather than a duration.

``` r
DateRange(start_date = DateCalendar(1956, 7, 26),
           end_date = DateCalendar(1956, 9, 15))
#> Date:     BET 26 JUL 1956 AND 15 SEP 1956
DateRange(start_date = DateCalendar(1956, 7, 26))
#> Date:     AFT 26 JUL 1956
DateRange(end_date = DateCalendar(1956, 9, 15))
#> Date:     BEF 15 SEP 1956
```

It’s important to note that approximate dates cannot be used in date
ranges or date periods.

## Date values

Date values can be defined with
[`DateValue()`](https://jl5000.github.io/gedcomS7/reference/DateValue.md)
and are a higher level of date expression than the elementary values
given above. A calendar/approximate/range/period date can be used in
conjunction with a date phrase (free text) and time. Instead of being
single values, they are GEDCOM substructures:

``` r
my_date <- DateValue(
  date = DateCalendar(year = 1999, month = 4, day = 5),
  date_phrase = "Someone's birthday",
  time = Time(hour = 13, minute = 28, second = 57)
)

my_date
#> Date:     5 APR 1999 13:28:57Z (Someone's birthday)

my_date@GEDCOM
#> [1] "0 DATE 5 APR 1999"           "1 TIME 13:28:57Z"           
#> [3] "1 PHRASE Someone's birthday"
```

It’s also possible to only define a DateValue through a phrase:

``` r
DateValue(date = "", date_phrase = "March 14th")@GEDCOM
#> [1] "0 DATE"              "1 PHRASE March 14th"
```

A
[`DateSorting()`](https://jl5000.github.io/gedcomS7/reference/DateSorting.md)
object is similar to
[`DateValue()`](https://jl5000.github.io/gedcomS7/reference/DateValue.md)
but it can only take Calendar dates as it is used for sorting.

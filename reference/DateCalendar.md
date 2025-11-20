# Create a GEDCOM Calendar Date object

Create a GEDCOM Calendar Date object

## Usage

``` r
DateCalendar(
  year = integer(0),
  month = integer(0),
  day = integer(0),
  julian = FALSE
)
```

## Arguments

- year:

  The year given as an integer (not 0). Negative years are interpreted
  as Before the Common Era (BCE). If BCE, only year should be provided.

- month:

  The month of the year given as an integer between 1 and 12.

- day:

  The day of the month given as an integer between 1 and 31.

- julian:

  Whether the date is given in the Julian calendar. If not, it is
  assumed to be in the Gregorian calendar.

## Value

An S7 object representing a GEDCOM Calendar Date.

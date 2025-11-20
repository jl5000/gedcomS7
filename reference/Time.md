# Create a time object

Create a time object

## Usage

``` r
Time(
  hour = integer(0),
  minute = integer(0),
  second = integer(0),
  fraction = integer(0),
  utc = TRUE
)
```

## Arguments

- hour:

  The hour of the day given as an integer between 0 and 23.

- minute:

  The minute of the hour given as an integer between 0 and 59.

- second:

  The second of the minute given as an integer between 0 and 59.

- fraction:

  The fraction of the second given as an integer.

- utc:

  Whether the time is in Coordinated Universal Time (UTC) (TRUE, the
  default) or is in local time (FALSE).

## Value

An S7 object representing a GEDCOM time.

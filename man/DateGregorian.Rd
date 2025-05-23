% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cls_base_dates.R
\name{DateGregorian}
\alias{DateGregorian}
\title{Create a GEDCOM Gregorian Date object}
\usage{
DateGregorian(
  year = integer(0),
  month = integer(0),
  day = integer(0),
  bce = FALSE
)
}
\arguments{
\item{year}{The year given as an integer (greater than 0).}

\item{month}{The month of the year given as an integer between 1 and 12.}

\item{day}{The day of the month given as an integer between 1 and 31.}

\item{bce}{Whether the date is Before the Common Era. This is FALSE by default,
but if TRUE, only the year should be given.}
}
\value{
An S7 object representing a GEDCOM Gregorian Date.
}
\description{
Create a GEDCOM Gregorian Date object
}

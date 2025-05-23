% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cls_base_dates.R
\name{DateSorting}
\alias{DateSorting}
\title{Create a GEDCOM Sorting Date object}
\usage{
DateSorting(
  date = character(0),
  date_phrase = character(0),
  time = character(0)
)
}
\arguments{
\item{date}{The date given either as a formatted GEDCOM string, or a
\code{DateGregorian} object.}

\item{date_phrase}{Textual information that cannot be expressed in the date.}

\item{time}{The time given either as a formatted GEDCOM string, or a
\code{Time()} object.}
}
\value{
An S7 object representing a GEDCOM Sorting Date.
}
\description{
Create a GEDCOM Sorting Date object
}

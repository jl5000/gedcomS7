% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cls_base_dates.R
\name{DateValue}
\alias{DateValue}
\title{Create a GEDCOM Date Value object}
\usage{
DateValue(date = character(0), date_phrase = character(0), time = character(0))
}
\arguments{
\item{date}{The date given either as a formatted GEDCOM string, or a
\code{DateValue()} object.}

\item{date_phrase}{Textual information that cannot be expressed in the date.}

\item{time}{The time given either as a formatted GEDCOM string, or a
\code{Time()} object.}
}
\value{
An S7 object representing a GEDCOM Date Value.
}
\description{
Create a GEDCOM Date Value object
}

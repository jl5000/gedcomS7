% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cls_change_date.R
\name{ChangeDate}
\alias{ChangeDate}
\title{Create a change date object}
\usage{
ChangeDate(
  date_exact = character(0),
  time = character(0),
  note_xrefs = character(0),
  notes = list()
)
}
\arguments{
\item{date_exact}{An exact date given either as a formatted GEDCOM string, or a
\code{DateExact()} object. If not given, it will default to today's date.}

\item{time}{The time given either as a formatted GEDCOM string, or a
\code{Time()} object.}

\item{note_xrefs}{A character vector of relevant note record cross-reference identifiers.}

\item{notes}{Associated notes. This can either be a \code{Note()} object, a list of them,
or a character vector of notes.}
}
\value{
An S7 object representing a GEDCOM CHANGE_DATE.
}
\description{
Create a change date object
}

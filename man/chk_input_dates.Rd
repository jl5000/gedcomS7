% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00_validators.R
\name{chk_input_dates}
\alias{chk_input_dates}
\title{Validate a date range/period}
\usage{
chk_input_dates(start_date, end_date)
}
\arguments{
\item{start_date}{The start date given either as a \code{DateGregorian} or a GEDCOM date string.}

\item{end_date}{The end date given either as a \code{DateGregorian} or a GEDCOM date string.}
}
\value{
Either a character string giving an error message, or NULL.
}
\description{
Validate a date range/period
}
\keyword{internal}

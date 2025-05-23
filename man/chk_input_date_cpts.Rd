% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00_validators.R
\name{chk_input_date_cpts}
\alias{chk_input_date_cpts}
\title{Validate a date by its components}
\usage{
chk_input_date_cpts(year, month, day, bce = FALSE)
}
\arguments{
\item{year}{The year.}

\item{month}{The month.}

\item{day}{The day.}

\item{bce}{Whether the date occurs before the common era.}
}
\value{
Either a character string giving an error message, or NULL.
}
\description{
Validate a date by its components
}
\keyword{internal}

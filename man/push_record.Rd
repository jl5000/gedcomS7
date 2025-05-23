% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_push_pull.R
\name{push_record}
\alias{push_record}
\title{Push an edited record back into a GEDCOM object}
\usage{
push_record(gedcom, record)
}
\arguments{
\item{gedcom}{An object representing the GEDCOM file.}

\item{record}{An object representing the record to place back into the
GEDCOM object.}
}
\value{
An updated GEDCOM object.
}
\description{
Push an edited record back into a GEDCOM object
}
\details{
The function will automatically keep family links for individuals updated.
It will also update the record last_updated if update_change_dates in the
gedcom object is set to TRUE.
}

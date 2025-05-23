% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io_export.R
\name{write_gedcom}
\alias{write_gedcom}
\title{Save a gedcom object to disk as a GEDCOM file}
\usage{
write_gedcom(
  gedcom,
  filepath = file.choose(),
  inc_confid = TRUE,
  inc_private = TRUE,
  inc_living = TRUE
)
}
\arguments{
\item{gedcom}{A gedcom object.}

\item{filepath}{The full filepath to write to.}

\item{inc_confid}{Whether to include records that are marked as confidential.}

\item{inc_private}{Whether to include records that are marked as private.}

\item{inc_living}{Whether to include individual records for suspected living people.}
}
\value{
The filepath (invisibly).
}
\description{
Save a gedcom object to disk as a GEDCOM file
}
\details{
This function prepares the gedcom object and then writes it to the filepath.
Steps taken include filtering sensitive data, escaping "@" signs (with another "@") ,
and splitting long lines onto separate lines.
}

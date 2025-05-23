% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_regex.R
\name{reg_date_exact}
\alias{reg_date_exact}
\title{Construct a regular expression for DATE_EXACT values}
\usage{
reg_date_exact(only = TRUE, strict = TRUE)
}
\arguments{
\item{only}{Whether to allow strings of only date_exact. If FALSE,
the regular expression accepts patterns where text can come before or after
the date_exact().}

\item{strict}{Whether to check whether the days are actually valid
or just check that days are any one or two digit number.}
}
\value{
A regex string
}
\description{
Construct a regular expression for DATE_EXACT values
}
\keyword{internal}

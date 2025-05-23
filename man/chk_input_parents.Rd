% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00_validators.R
\name{chk_input_parents}
\alias{chk_input_parents}
\title{Validate the existence of an input's parent value}
\usage{
chk_input_parents(input, name, parent, parent_name)
}
\arguments{
\item{input}{The input.}

\item{name}{The input name.}

\item{parent}{The parent input.}

\item{parent_name}{The name of the parent input used in any error messages.}
}
\value{
Either a character string giving an error message, or NULL.
}
\description{
Validate the existence of an input's parent value
}
\keyword{internal}

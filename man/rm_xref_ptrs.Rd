% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_add_rm.R
\name{rm_xref_ptrs}
\alias{rm_xref_ptrs}
\title{Remove xref pointers from GEDCOM lines}
\usage{
rm_xref_ptrs(lines, xref, void_refs)
}
\arguments{
\item{lines}{A character vector of GEDCOM lines.}

\item{xref}{The xref to remove.}

\item{void_refs}{Whether to replace references to the xref with
a @VOID@ reference, or remove the structure entirely.}
}
\value{
The GEDCOM lines without pointers to the xref.
}
\description{
Remove xref pointers from GEDCOM lines
}
\keyword{internal}

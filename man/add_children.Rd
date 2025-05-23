% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_add_rm.R
\name{add_children}
\alias{add_children}
\title{Create children records for a family}
\usage{
add_children(x, xref, sexes, chil_names = NULL)
}
\arguments{
\item{x}{A gedcom object.}

\item{xref}{The xref of a Family record.}

\item{sexes}{A character string giving the sexes of each child. For example,
"FFM" to add two daughters and one son.}

\item{chil_names}{A character vector of children's names. If provided, it must be
the same length as the number of sexes. If you don't want to provide a name for a
child, set the name to "".

Surnames must be enclosed in forward slashes. If all names you supply do not
contain forward slashes then surnames will be taken from the father (or mother).}
}
\value{
A gedcom object with additional child records.
}
\description{
Create children records for a family
}

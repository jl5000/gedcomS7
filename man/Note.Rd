% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cls_note.R
\name{Note}
\alias{Note}
\title{Create a note structure object}
\usage{
Note(
  text = character(0),
  language = character(0),
  media_type = character(0),
  translations = list(),
  citations = list()
)
}
\arguments{
\item{text}{A character string. New lines are created with \\n.}

\item{language}{A character string of language tags as defined in BCP 47.}

\item{media_type}{The media type as defined in RFC 2045.}

\item{translations}{A \code{TranslationText()} object or a list of them.
One for each alternate translation of the text.}

\item{citations}{Associated sources. This can either be a \code{SourceCitation()} object, a list of them,
or a character vector of XREFs of source records.}
}
\value{
An S7 object representing a GEDCOM NOTE_STRUCTURE.
}
\description{
Create a note structure object
}
\details{
The shared note (SNOTE) alternative of this structure is defined
separately in relevant structures.
}

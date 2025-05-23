% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cls_media_link.R
\name{MediaLink}
\alias{MediaLink}
\title{Create a multimedia link object}
\usage{
MediaLink(
  media_xref = "@VOID@",
  title = character(0),
  top = integer(0),
  left = integer(0),
  height = integer(0),
  width = integer(0)
)
}
\arguments{
\item{media_xref}{The cross-reference identifier of a multimedia record.}

\item{title}{The title of the multimedia record.}

\item{top}{The number of pixels to omit from the top side of the image.}

\item{left}{The number of pixels to omit from the left side of the image.}

\item{height}{The height in pixels of the cropped region.}

\item{width}{The width in pixels of the cropped region.}
}
\value{
An S7 object representing a GEDCOM MULTIMEDIA_LINK.
}
\description{
Create a multimedia link object
}
\details{
The properties @left and @top indicate the top left corner of the region to display.
The properties @width and @height indicate the dimensions of the region to display.

If the multimedia record contains multiple files, then the crop parameters only
applies to the first file.
}

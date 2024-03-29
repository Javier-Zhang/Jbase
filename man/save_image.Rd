% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_image.R
\name{save_image}
\alias{save_image}
\title{Quick Save Images}
\usage{
save_image(dir = "./", name = "Test", width = 5, height = 5, save_as = "pdf")
}
\arguments{
\item{dir}{Dir to save figure.}

\item{name}{Figure name}

\item{width}{Figure width}

\item{height}{Figure height}

\item{save_as}{Figure format: png, pdf, svg.}
}
\value{
Image in the local folder.
}
\description{
Quick Save Images
}
\examples{
save_image(dir = "./", name = "test", width = 5, height = 5, save_as = "pdf")
}

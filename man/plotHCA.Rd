\name{plotHCA}
\alias{plotHCA}
\title{
Plot Dendrogram for Spectra Object
}
\description{
This function plots the results of an HCA analysis of a \code{"Spectra"} object.  This is not called directly by the user -- \code{\link{hcaSpectra}} and \code{\link{hcaScores}} use it (see those pages for examples).
}
\usage{
plotHCA(spectra, hclst, sub.title, use.sym, ...)
}
\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}.
}
  \item{hclst}{
A \code{\link{hclust}} object.
}

  \item{sub.title}{
A character string for the subtitle.
}
  \item{use.sym}{
Logical; if true, the color scheme will be black and lower-case letters will be used to indicate group membership.
}
  \item{\dots}{
Additional parameters to be passed to the plotting routines.
}
}
\value{
An object of class \code{\link{dendrogram}}. Side effect is a plot.
}


\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\keyword{ hplot }
\keyword{ multivariate }
\keyword{ cluster }

\name{plotHCA}
\alias{plotHCA}
\title{
Plot Dendrogram for Spectra Object
}
\description{
This function plots the results of an HCA analysis of a \code{"Spectra"} object.  This is not called directly by the user -- \code{\link{hcaSpectra}} and \code{\link{hcaScores}} use it (see those pages for examples).
}
\usage{
plotHCA(spectra, distance, title, sub.title, method, use.sym, ...)
}
\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}.
}
  \item{sub.title}{
A character string for the subtitle.
}
  \item{distance}{
A distance matrix.
}
  \item{title}{
A character string for the title.
}
  \item{method}{
A method acceptable to the \code{\link{hclust}} function.
}
  \item{use.sym}{
Logical; if true, the color scheme will be black and lower-case letters will be used to indicate group membership.
}
  \item{\dots}{
Additional parameters to be passed to the plotting routines.
}
}
\value{
None; side effect is a plot.
}


\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\keyword{ hplot }
\keyword{ multivariate }
\keyword{ cluster }
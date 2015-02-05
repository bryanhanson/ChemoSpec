\name{covSpectra}
\alias{covSpectra}

\title{
Plot something...
}

\description{
Plot something...
}

\usage{
covSpectra(spectra, freq = spectra$freq[1], C = NULL, V = NULL, ...)
}

\arguments{
  \item{spectra}{
An object of S3 class \code{\link{Spectra}}.
}
  \item{freq}{
Numeric.  The desired frequency to be plotted.
}
  \item{C}{
Matrix.  Optional. A precomputed correlation matrix.
}
  \item{V}{
Matrix.  Optional. A precomputed covariance matrix.
}
  \item{\dots}{
Other parameters to be passed to the plotting functions.
}
}

\value{
A list giving the covariance and correlation matrices.  A plot will also be made if \code{plot = TRUE}.
}

\note{
The calculation of the correlation and covariance matrices may take quite some time for large data sets.  The plotting is also slow in these cases.  The base graphics options (\code{pmode = "contour"} or \code{"image"} are much faster).  DISCUSS COLOR SCALE.
}

\references{
\url{https://github.com/bryanhanson/ChemoSpec}
ADD NICHOLSON REFERENCE
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}


\seealso{
\code{\link{covSpectra}}, \code{\link{chooseLvls}} to select levels automatically.
}

\details{
If \code{spectra$freq} is in decreasing order, it and \code{spectra$data} are silently re-ordered to be ascending before plotting.
}

\examples{
data(metMUD2)
res <- corSpectra(metMUD2)
}

\keyword{ plot }

\name{corSpectra}
\alias{corSpectra}

\title{
STOCSY analysis of a Spectra object
}

\description{
Carries out a STOSCY analysis of a \code{Spectra} object, using the method of Nicholson.
}

\usage{
corSpectra(spectra, plot = TRUE, limX = NULL, limY = NULL, nticks = 10,
levels = NULL, pmode = "contour", C = NULL, V = NULL, ...)
}

\arguments{
  \item{spectra}{
An object of S3 class \code{\link{Spectra}}
}
  \item{plot}{
Logical.  Should a plot be made?
}
  \item{limX}{
Numeric vector of length 2.  The x limits.
}
  \item{limY}{
Numeric vector of length 2.  The y limits.
}
  \item{nticks}{
Integer.  The number of ticks to be drawn.
}
  \item{levels}{
Numeric.  A vector of values at which to draw the contours or levels.
}
  \item{pmode}{
Character.  The plot mode. One of \code{c("contour", "image", "contourplot", "levelplot")}. rgl is a possibility too. exCon also.
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

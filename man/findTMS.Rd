\name{findTMS}
\alias{findTMS}
\title{
Find the TMS/TSP Peak in an NMR Spectrum.
}
\description{
This is a simple function to find the TMS or TSP peak in proton NMR spectrum.  It simply finds the rightmost large peak in the spectrum.
}
\usage{
findTMS(x, span, sn, thresh = 0.2)
}
\arguments{
  \item{x}{
Vector of intensities of the spectrum.  The frequency aspect is handled simply as the index along the intensity vector.
}
  \item{span}{
Integer; the number of data points to use as a window on either side of a peak when calculating the SN.  See the code for exactly how this window is created.  Larger values find fewer peaks.
}
  \item{sn}{
Numeric; the minimum value of the signal-to-noise ratio for detecting a peak.
}
  \item{thresh}{
Numeric; a value in 0...1 which gives the quantile of peaks in the entire spectrum which should be kept on the short list for finding the TMS peak.
}
}
\details{
The rightmost peak is sought, so the spectrum must be presented in the usual high ppm --> low ppm format.  Calls \code{\link{calcSN}} which will issue warnings when peaks are near the edges of the spectrum.
}
\value{
The index of the peak believed to be the TMS peak.
}
\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
# Some test data:
set.seed(99)
vec <- abs(rnorm(100))
vec[12] <- 10
vec[75] <- 31 # create two larger peaks

ans <- findTMS(vec, 20, 5)
ans # index as set above (75)
plot(vec, type = "l")
abline(v = ans, col = "red")

}
\keyword{ utilities }

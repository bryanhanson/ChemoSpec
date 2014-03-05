\name{calcSN}
\alias{calcSN}
\title{
Calculate the signal-to-noise ratio of a peak in a spectrum.
}
\description{
This is very simple function to calculate the signal-to-noise ratio of a peak in a spectrum.  It is intended for processing NMR spectra in conjunction with \code{\link{findTMS}}.  It is not extensively tested.  The SN value returned is just one (simple) way of computing SN.  You probably shouldn't trust it.
}
\usage{
calcSN(x, span, idx, debug = FALSE)
}
\arguments{
  \item{x}{
Vector of intensities of the spectrum.  The frequency aspect is handled simply as the index along the intensity vector.
}
  \item{span}{
Integer; the number of data points to use as a window on either side of a peak when calculating the SN.  See the code for exactly how this window is created.  Larger values find fewer peaks.
}
  \item{idx}{
Integer; the index giving the location of the peak to have its SN computed.
}
  \item{debug}{
Logical; Spew forth information, more than you want.
}
}
\details{
Warnings are issued when peaks are near the edges of the spectrum.  The algorithm computes the SN for these peaks differently, and possibly inaccurately.  You are more likely to get the warning with higher values of \code{span}.  See the example.
}
\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
# Function to pick peaks:
pp <- function (x, span) {
	# Borrowed from ChemometricsWithR by R. Wehrens
	span.width <- span * 2 + 1
	loc.max <- span.width + 1 - apply(embed(x, span.width), 1, 
        which.max)
	loc.max[loc.max == 1 | loc.max == span.width] <- NA
	pks <- loc.max + 0:(length(loc.max) - 1)
	unique(pks[!is.na(pks)])
	}
# Some test data:
set.seed(99)
vec <- abs(rnorm(100))
vec[12] <- 10
vec[75] <- 31 # create two larger peaks

# Now demonstrate:
span <- 20
ppVec <- pp(vec, span)
for (i in 1:length(ppVec)) {
	tmp <- calcSN(vec, span, ppVec[i])
	print(tmp) # SN of each peak found reported
}
plot(vec, type = "l")
abline(v = ppVec, col = "red") # Shows peaks identified
# Try repeating with span = 10
}
\keyword{ utilities }

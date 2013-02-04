\name{removeFreq}
\alias{removeFreq}
\title{Remove Frequencies from a Spectra Object}

\description{
This function removes specified frequencies from a \code{"Spectra"} object.  For instance, one might want to remove regions lacking any useful information (to reduce the data size), or remove regions with large interfering peaks (e.g. the water peak in 1H NMR).
}

\usage{
removeFreq(spectra, rem.freq)
}
\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"} from which to remove selected frequencies.
}
  \item{rem.freq}{A valid R statement describing the frequencies to be removed.  This must comply with \code{\link{Comparison}} and \code{\link{Logic}}.  See the examples below for common usage.
}
}
\details{
rem.freq can be any valid \code{R} statement that leads to a vector of logicals.  In the examples below, the | and \& operators seem backward in some sense, but R evaluates them one at a time and combines the result to give the required output.
}

\value{
An object of S3 class \code{"Spectra"}.
}


\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
data(CuticleIR)
sumSpectra(CuticleIR) # note the frequency range & existing gap
# remove frequencies from one end:
newIR <- removeFreq(CuticleIR, rem.freq = CuticleIR$freq > 3500)
# remove frequencies from both ends at once:
newIR <- removeFreq(CuticleIR, rem.freq = CuticleIR$freq > 3500
| CuticleIR$freq < 800)
# remove frequencies from the middle:
newIR <- removeFreq(CuticleIR, rem.freq = CuticleIR$freq > 800
& CuticleIR$freq < 1000)

# The logic of this last one is as follows.  Any values
# that are TRUE will be removed.
values <- 1:7
values > 2
values < 6
values > 2 & values < 6

# after any of these, inspect the results:
sumSpectra(newIR)
check4Gaps(newIR$freq, newIR$data[1,], plot = TRUE)
}
\keyword{ utilities }
\keyword{ manip }

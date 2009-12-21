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
  \item{rem.freq}{A list of frequencies to be removed.  Some rounding may occur.
}
}
\details{
To remove the extreme values, some experimentation with the max/min value in rem.freq may be necessary due to rounding.  You can check your success by using \code{\link{check4Gaps}} and looking for an orphaned value or two.
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
sumSpectra(CuticleIR) # note the frequency range & gaps
res <- removeFreq(CuticleIR, rem.freq = c(2850:3050)) # remove hydrocarbon peaks
sumSpectra(res)

}
\keyword{ utilities }
\keyword{ manip }
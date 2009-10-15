\name{trimNbin}
\Rdversion{1.1}
\alias{trimNbin}

\title{
Edit and Bin Frequencies in a Spectra Object
}

\description{
Function to remove specified frequencies from a \code{"Spectra"} object, for instance, noisy or uninformative regions.  The bin function is currently not implemented.
}
\usage{
trimNbin(spectra, rem.freq)
}

\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}
}
  \item{rem.freq}{
An integer vector of frequencies to be removed. See details.
}
}

\details{
Frequencies should be given as integers.  A certain amount of rounding will occur.  A specification such as \code{rem.freq = c("min", 1500)} will remove all values from the minimum up to 1500.  See the examples for further valid specifications.  If \code{"min"} or \code{"max"} are used, the action is to create a range of frequencies to remove with the adjacent numerical value.
}
\value{
A modified object of S3 class \code{"Spectra"}
}

\seealso{
\code{\link{removeSample}} to remove one or more samples from a \code{"Spectra"} object.
}
\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\section{Calls}{\code{\link{chkSpectra}}}
\section{Called by}{Top level function, called by user.}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
data(CuticleIR)
new1 <- trimNbin(CuticleIR, rem.freq = c(750:1000)) # removes all frequencies in the range
new2 <- trimNbin(CuticleIR, rem.freq = c("min", 1000)) # removes all frequencies from the minimum through 1000
new3 <- trimNbin(CuticleIR, rem.freq = c("min", 1000, 3000, "max")) # frequencies between 1000 and 3000 remain
new4 <- trimNbin(CuticleIR, rem.freq = c("min", 1000, 2000:2500, 3000, "max")) # frequencies between 1000 and 2000, 2500 and 3000 remain
\dontrun{new5 <- trimNbin(CuticleIR, rem.freq = c("min", 1000 "max")) # silliness, removes everything!}

}

\keyword{ utilities }


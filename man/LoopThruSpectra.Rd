\name{loopThruSpectra}
\alias{loopThruSpectra}
\title{
Display the spectra in a Spectra object one at a time}
\description{
Plots each spectrum in a \code{Spectra} object one at a time (waits for a return in the console before plotting the next spectrum).  Use \code{ESC} to get out of the loop.
}
\usage{
loopThruSpectra(Spectra, ...)
}
\arguments{
  \item{Spectra}{An object of S3 class \code{"Spectra"}.
}
  \item{\dots}{
Parameters to be passed downstream.
}
}
\value{
None. Side effect is a plot.
}
\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
\dontrun{
data(metMUD1)
loopThruSpectra(metMUD1)
}
}
\keyword{ hplot }

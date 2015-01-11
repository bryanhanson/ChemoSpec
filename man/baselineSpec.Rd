\name{baselineSpec}
\alias{baselineSpec}
\title{
Carry out baseline correction on a Spectra object
}
\description{
This function is a simple wrapper to functions in package \code{baseline} which carries out a variety of baseline correction routines.
}
\usage{
baselineSpec(Spectra, int = TRUE, retC = FALSE, ...)
}
\arguments{
  \item{Spectra}{
An object of S3 class \code{"Spectra"} to be checked.
}
  \item{int}{
Logical; if \code{TRUE}, do the correction interactively using widgets.  No results are saved.  Use this for inspection and exploration only.
}
  \item{retC}{
Logical: shall the baseline-corrected spectra be returned in the \code{Spectra} object?
}
  \item{\dots}{
Other arguments passed downstream.  The relevant ones can be found in \code{\link[baseline]{baseline}}.  Be sure to pay attention to argument \code{method} as you will probably want to use it.
}
}
\value{
If \code{int = TRUE}, an interactive plot is created.  If \code{int = FALSE} and \code{retC = FALSE}, an object of class \code{baseline} is returned (see \code{\link[baseline]{baseline-class}}.  If \code{int =  FALSE} and \code{retC = TRUE}, a \code{Spectra} object containing the corrected spectra is returned.  In these latter two cases plots are also drawn.
}
\details{In the plots, the x axis ticks give the data point index, not the original values from your data. Note that you cannot zoom the non-interactive display of corrected spectra because the underlying function hardwires the display.  Try the interactive version instead (\code{int = TRUE}).
}
\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
data(SrE.IR)
temp <- baselineSpec(SrE.IR, int = FALSE, method = "rfbaseline")
par(mfrow = c(1,1)) # needed to cancel 2 panel plot
\dontrun{
baselineSpec(SrE.IR)
par(mfrow = c(1,1)) # needed to cancel 2 panel plot
}
}

\keyword{ hplot }

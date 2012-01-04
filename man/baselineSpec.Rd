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
Other arguments passed downstream.  The relevant ones apply mainly when \code{int = FALSE} and can be found in \code{\link[baseline]{baseline}}.
}
}
\value{
If \code{int = FALSE}, an interactive plot is created.  If \code{int = FALSE} and \code{retC = FALSE}, an object of class \code{baseline} is returned (see \code{\link[baseline]{baseline-class}}.  If \code{int =  FALSE} and \code{retC = TRUE}, a \code{Spectra} object containing the corrected spectra is returned.  In these latter two cases plots are also drawn.
}
\details{In the plots, the x axis ticks give the data point index, not the original values from your data.
}
\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
library("IDPmisc")
data(SrE.IR)
temp <- baselineSpec(SrE.IR, int = FALSE, method = "rfbaseline")
\dontrun{
baselineSpec(SrE.IR)
}
}

\keyword{ plot }

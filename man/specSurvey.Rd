\name{specSurvey}
\Rdversion{1.1}
\alias{specSurvey}

\title{
Plot Std Dev of Frequencies in a Spectra Object
}
\description{
Computes the standard deviation of each frequency value for all the spectra in a \code{"Spectra"} object and plots it against the frequency.  Useful for identifying uninformative spectral regions.
}
\usage{
specSurvey(spectra, title = "No title provided", ...)
}

\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}.
}
  \item{title}{
A character string for the plot title.
}

  \item{\dots}{
Additional parameters to be passed to the plotting functions.}
}

\value{
None.  Side effect is a plot.
}

\section{Calls}{\code{\link{chkSpectra}}}
\section{Called by}{Top level function, called by user.}

\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
data(CuticleIR)
specSurvey(CuticleIR, title = "Cuticle IR Spectra")
}

\keyword{ multivariate }

\name{sumSpectra}
\Rdversion{1.1}
\alias{sumSpectra}

\title{
Summarize a Spectra Object
}
\description{
Provides a summary of a \code{"Spectra"} object, essentially a more spectroscopist-friendly version of \code{str()}.
}

\usage{
sumSpectra(spectra)
}

\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}.
}
}
\details{
Prior to summarizing, \code{\link{chkSpectra}} is run with confirm = FALSE.  If there are problems, warnings are issued to the console and the summary is not done.
}

\value{
None.  Results printed at console, perhaps a plot as well.
}


\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
data(CuticleIR)
sumSpectra(CuticleIR)

}

\keyword{ utilities }


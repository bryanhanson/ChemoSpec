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
sumSpectra(spectra, ...)
}

\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}.
}
  \item{...}{
Arguments to be passed downstream.
}

}
\details{
Prior to summarizing, \code{\link{chkSpectra}} is run with confirm = FALSE.  If there are problems, warnings are issued to the console and the summary is not done.  If \code{sumSpectra} thinks there is a gap between every data point, add the argument \code{tol = xx} which will pass through to \code{\link{check4Gaps}} and alleviate this problem (which has to do with rounding when subtracting two adjacent frequency values).  The \code{"Spectra"} object is checked to see if it contains data elements beyond what is required.  If so, these extra elements are reported to the console.
}

\value{
None.  Results printed at console, perhaps a plot as well.
}


\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
data(CuticleIR)
sumSpectra(CuticleIR)

}

\keyword{ utilities }


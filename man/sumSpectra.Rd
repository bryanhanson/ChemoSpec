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
\item{\dots}{Arguments to be passed downstream.  See details.
}
}

\details{
Prior to summarizing, \code{\link{chkSpectra}} is run with confirm = FALSE.  If there are problems, warnings are issued to the console and the summary is not done.  If issued with \code{plot = TRUE}, this is passed via the \dots to \code{\link{check4Gaps}} which then produces a plot showing any gaps in the data.  Cool.
}

\value{
None.  Results printed at console, perhaps a plot as well.
}

\section{Calls}{\code{\link{chkSpectra}}, \code{\link{check4Gaps}}}
\section{Called by}{Top level function, called by user.}

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


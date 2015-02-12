\name{cv_pcaSpectra}
\Rdversion{1.1}
\alias{cv_pcaSpectra}

\title{
Cross-Validation of Classical PCA Results for a Spectra Object
}
\description{
This function carries out classical PCA on the data in a \code{\link{Spectra}} object using a cross-validation method.  Nothing more than a wrapper to Peter Filzmoser's \code{\link[chemometrics]{pcaCV}} method with some small plotting changes.
}
\usage{
cv_pcaSpectra(spectra, pcs, choice = "noscale", repl = 50,
	segments = 4, segment.type = c("random", "consecutive", "interleaved"),
	length.seg, trace = FALSE, ...)
}
\arguments{
  \item{spectra}{
An object of S3 class \code{\link{Spectra}}.
}
  \item{choice}{A character string indicating the choice of scaling.  One of \code{c("noscale", "autoscale", "Pareto").}
}
  \item{pcs}{
As per \code{\link[chemometrics]{pcaCV}} where it is called amax; an integer giving the number of PC scores to include.
}
  \item{repl}{
As per \code{\link[chemometrics]{pcaCV}}; the number of replicates to perform.
}
  \item{segments}{
As per \code{\link[chemometrics]{pcaCV}}.
}
  \item{segment.type}{
As per \code{\link[chemometrics]{pcaCV}}.
}
  \item{length.seg}{
As per \code{\link[chemometrics]{pcaCV}}.
}
  \item{trace}{
As per \code{\link[chemometrics]{pcaCV}}.
}
  \item{\dots}{
Parameters to be passed to the plotting routines.
}
}
\value{
A list as described in \code{\link[chemometrics]{pcaCV}}, so the result must be assigned or it will appear at the console.  Side effect is a plot.
}


\references{
K. Varmuza and P. Filzmoser \emph{Introduction to Multivariate Statistical Analysis in Chemometrics}, CRC Press, 2009.

\url{https://github.com/bryanhanson/ChemoSpec}\cr

}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
\code{\link[chemometrics]{pcaCV}} for the underlying function.
}

\examples{
data(SrE.IR)
results <- cv_pcaSpectra(SrE.IR, pcs = 5)
}

\keyword{ multivariate }

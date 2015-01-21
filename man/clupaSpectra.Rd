\name{clupaSpectra}

\alias{clupaSpectra}

\title{
Conduct Hierarchical Cluster-Based Peak Alignment on a Spectra Object
}

\description{
This function is a wrapper to several functions in the \code{speaq} package.  It implements the "CluPA" algorithm described in the reference.
}

\usage{
clupaSpectra(spectra, bT = NULL, ...)
}

\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}
}

  \item{bT}{
Numeric.  The baseline threshold. Defaults to five percent of the range of the data, in \code{spectra$data}.  Passed to \code{detectSpecPeaks}.
}

  \item{\dots}{
Other arguments to be passed to the underlying functions.
}
}

\value{
A modifed \code{"Spectra"} object.
}

\references{
\url{https://github.com/bryanhanson/ChemoSpec}

Vu TN, Valkenborg D, Smets K, Verwaest KA, Dommisse R, Lemiere F, Verschoren A, Goethals B, Laukens K. (2011) An integrated workflow for robust alignment and simplified quantitative analysis of NMR spectrometry data. BMC Bioinformatics. 2011 Oct 20;12:405.
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
\dontrun{
# A bit slow for CRAN checks
require("speaq")
data(SrE.NMR)
tst <- clupaSpectra(SrE.NMR)
}
}
\keyword{ utilities }

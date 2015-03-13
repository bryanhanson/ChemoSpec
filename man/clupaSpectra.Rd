\name{clupaSpectra}

\alias{clupaSpectra}

\title{
Conduct Hierarchical Cluster-Based Peak Alignment on a Spectra Object
}

\description{
This function is a wrapper to several functions in the \code{speaq} package.  It implements the CluPA algorithm described in the reference.
}

\usage{
clupaSpectra(spectra, bT = NULL, ...)
}

\arguments{
  \item{spectra}{
An object of S3 class \code{\link{Spectra}}.
}

  \item{bT}{
Numeric.  The baseline threshold. Defaults to five percent of the range of the data, in \code{spectra$data}.  Passed to \code{detectSpecPeaks}.
}

  \item{\dots}{
Other arguments to be passed to the underlying functions.
}
}

\value{
A modifed \code{\link{Spectra}} object.
}

\references{

Vu TN, Valkenborg D, Smets K, Verwaest KA, Dommisse R, Lemiere F, Verschoren A, Goethals B, Laukens K. "An integrated workflow for robust alignment and simplified quantitative analysis of NMR spectrometry data" BMC Bioinformatics vol. 12 pg. 405 (2011).

\url{https://github.com/bryanhanson/ChemoSpec}

}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
data(alignMUD)
plotSpectra(alignMUD, which = 1:20, lab.pos = 4.5, offset = 0.1,
	yrange = c(0, 1900), amp = 500, xlim = c(1.5, 1.8),
	main = "Misaligned NMR Spectra (alignMUD)")
aMUD <- clupaSpectra(alignMUD)
plotSpectra(aMUD, which = 1:20, lab.pos = 4.5, offset = 0.1,
	yrange = c(0, 1900), amp = 500, xlim = c(1.5, 1.8),
	main = "Aligned NMR Spectra (alignMUD)")
}
\keyword{ utilities }

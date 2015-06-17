\name{hmapSpectra}
\alias{hmapSpectra}

\title{
Create a Seriated Heat Map Comparing Samples and Spectral Data for a Spectra Object
}
\description{
Creates a heat map with marginal dendrograms using seriation procedures.  Very briefly, the samples that are most like each other occur in one corner, and the frequencies that are most informative with respect to the samples are in that corner as well. This is achieved by using hierchical cluster analysis and then re-ordering the clusters in a coordinated way across each dimension.  See the reference.
}
\usage{
hmapSpectra(spectra, ...)
}

\arguments{
  \item{spectra}{ An object of S3 class \code{\link{Spectra}}.}

  \item{\dots}{
Additional arguments to be passed downstream.  A great deal of control is available - check \code{?hmap} for details.
}
}
\value{
A list as described in \code{?hmap}.  Side effect is a plot.
}
\references{
\url{https://github.com/bryanhanson/ChemoSpec}

}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
\code{\link[seriation]{hmap}} which will get you to the package (there is no package index page); the vignette is a good place to begin (\code{browseVignettes("seriation")}).
}

\examples{
data(SrE.IR)
# remove noisy, uninteresting region:
newIR <- removeFreq(SrE.IR, rem.freq =
SrE.IR$freq > 1800 & SrE.IR$freq < 2500)
# now make the heat map:
res <- hmapSpectra(newIR)
}
\keyword{ multivariate }


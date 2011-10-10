\name{normSpectra}
\alias{normSpectra}
\title{Normalize a Spectra Object}

\description{
This function currently has one option for normalization, and that is \code{ti} for total intensity.  In this case, the y-data of a \code{"Spectra"} object is normalized by dividing each y-value by the sum of the y-values in a given spectrum.  This each spectrum sums to 1.  This method assumes that the total concentration of substances giving peaks does not vary across samples which may not be true.
}

\usage{
normSpectra(spectra, method = "ti")
}

\arguments{
  \item{spectra}{ An object of S3 class \code{"Spectra"} to be normalized.}
  \item{method}{ One of \code{c("ti")} giving the method for normalization.}
}

\value{
An object of S3 class \code{"Spectra"}.
}


\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
data(CuticleIR)
res <- normSpectra(CuticleIR)
sumSpectra(res)
}

\keyword{ utilities }
\keyword{ manip }
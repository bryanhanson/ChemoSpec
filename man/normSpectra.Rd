\name{normSpectra}
\alias{normSpectra}
\title{Normalize a Spectra Object}

\description{
This function takes the y-data of a \code{"Spectra"} object and normalizes it by dividing each y-value by the sum of the y-values in a given spectrum.
}

\usage{
normSpectra(spectra)
}

\arguments{
  \item{spectra}{ An object of S3 class \code{"Spectra"} to be normalized.}
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
\name{sampleDistSpectra}
\alias{sampleDistSpectra}

\title{
Compute the Distance Between Samples in a Spectra Object
}

\description{
Compute the Distance between samples in a Spectra object.  This is a means to quantify the similarity between samples.  A heat map style plot is an option.
}

\usage{
sampleDistSpectra(spectra, method = "pearson", plot = TRUE, ...)
}
\arguments{
  \item{spectra}{
An object of S3 class \code{\link{Spectra}}.
}
  \item{method}{
Character.  A string giving the distance method.  See \code{\link{rowDist}} for options.
}
  \item{plot}{
Logical.  Shall a level plot be made?
}
  \item{\dots}{
Arguments to be passed to the plotting function.
}
}

\value{
A numeric matrix giving the correlation coefficients.
}

\seealso{
The sample distances can be used to cluster the samples. See for example \code{\link{hcaSpectra}}.
}

\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
require("lattice")
data(metMUD2)
sampleDistSpectra(metMUD2, main = "Sample Correlations for metMUD2")
}
\keyword{ hplot }

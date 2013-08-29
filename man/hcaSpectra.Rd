\name{hcaSpectra}
\Rdversion{1.1}
\alias{hcaSpectra}

\title{
Plot HCA Results of a Spectra Object
}
\description{
A wrapper which carries out HCA and plots a dendrogram colored by the information in a \code{"Spectra"} object.  All methods for computing the cluster distances are available.
}
\usage{
hcaSpectra(spectra, title = "no title provided",
c.method = "complete", d.method = "euclidean", use.sym = FALSE, ...)
}
\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}.
}
  \item{title}{
A character string for the plot title.
}
  \item{c.method}{
A character string describing the clustering method; must be acceptable to \code{\link{hclust}}.
}
  \item{d.method}{
A character string describing the distance calculation method; must be acceptable as a method in \code{\link{rowDist}}.
}

  \item{use.sym}{
A logical; if true, use no color and use lower-case letters to indicate group membership.
}
  \item{\dots}{
Other parameters to be passed to the plotting functions.
}
}
\value{
None.  The side effect is a plot.
}


\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
\code{\link{hclust}} for the underlying function.
\code{\link{hcaScores}} for similar analysis of PCA scores from a \code{"Spectra"} object.
\code{\link{plotHCA}} for the function that actually does the plotting.
}
\examples{
data(SrE.IR)
myt <- expression(italic(S.)~italic(repens)~IR~Spectra)
hcaSpectra(SrE.IR, title = myt)
}
\keyword{ multivariate }
\keyword{ cluster }

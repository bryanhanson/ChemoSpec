\name{hcaScores}
\Rdversion{1.1}
\alias{hcaScores}

\title{
HCA on PCA scores from a Spectra Object
}
\description{
A wrapper which performs HCA on the scores from a PCA of a \code{"Spectra"} object, color-coding the results as specified in the object.
}

\usage{
hcaScores(spectra, pca, title = "no title provided",
	scores = c(1:5), c.method = "complete",
	d.method = "euclidean", use.sym = FALSE, ...)
}

\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}.
}
  \item{pca}{
An object of class \code{\link{prcomp}}, modified to include a list element called \code{$method}, a character string describing the pre-processing carried out and the type of PCA performed (it appears on the plot).  This is automatically provided if \code{ChemoSpec} functions \code{\link{classPCA}} or \code{\link{robPCA}} were used to create \code{pca}.
}
  \item{title}{
A character string for the plot title.}
  \item{scores}{
A vector of integers specifying which scores to use for the HCA.}
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
Additional parameters to be passed to the plotting functions.
}
}

\value{
None.  Side effect is a plot.
}


\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
\code{\link{hclust}} for the underlying function.
See \code{\link{hcaSpectra}} for HCA of the entire data set stored in the \code{"Spectra"} object.
\code{\link{plotHCA}} for the function that actually does the plotting.

}
\examples{
data(CuticleIR)
results <- classPCA(CuticleIR, choice = "noscale")
hcaScores(CuticleIR,  results, scores = c(1:5), title = "Cuticle IR Spectra")
}

\keyword{ multivariate }
\keyword{ cluster }

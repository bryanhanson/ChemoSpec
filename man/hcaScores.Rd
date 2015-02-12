\name{hcaScores}
\Rdversion{1.1}
\alias{hcaScores}

\title{
HCA on PCA scores from a Spectra Object
}
\description{
A wrapper which performs HCA on the scores from a PCA of a \code{\link{Spectra}} object, color-coding the results as specified in the object.  Many methods for computing the clusters and distances are available.
}

\usage{
hcaScores(spectra, pca, scores = c(1:5), c.method = "complete",
	d.method = "euclidean", use.sym = FALSE, ...)
}

\arguments{
  \item{spectra}{
An object of S3 class \code{\link{Spectra}}.
}
  \item{pca}{
An object of class \code{\link{prcomp}}, modified to include a list element called \code{$method}, a character string describing the pre-processing carried out and the type of PCA performed (it appears on the plot).  This is automatically provided if \code{ChemoSpec} functions \code{\link{c_pcaSpectra}} or \code{\link{r_pcaSpectra}} were used to create \code{pca}.
}
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
A list, containing an object of class \code{\link{hclust}} and an object of class \code{\link{dendrogram}}.  The side effect is a plot.
}


\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
\code{\link{hclust}} for the underlying function.
See \code{\link{hcaSpectra}} for HCA of the entire data set stored in the \code{\link{Spectra}} object.
\code{\link{plotHCA}} for the function that actually does the plotting.

}
\examples{
data(SrE.IR)
results <- c_pcaSpectra(SrE.IR, choice = "noscale")
myt <- expression(bolditalic(Serenoa)~bolditalic(repens)~bold(IR~Spectra))
res <- hcaScores(SrE.IR,  results, scores = c(1:5), main = myt)
}

\keyword{ multivariate }
\keyword{ cluster }

\name{mclustSpectra}
\alias{mclustSpectra}
\title{
mclust Analysis of a Spectra Object PCA Results
}
\description{
This function is a wrapper for the \code{Mclust} function and associated plotting functions.
}
\usage{
mclustSpectra(spectra, pca, pcs = c(1:3), dims = c(1, 2),
	title = "no title provided", plot = c("BIC", "proj", "error"),
	use.sym = FALSE, ...)
}
\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}.
}
  \item{pca}{
An object of class \code{\link{prcomp}}.
}
  \item{pcs}{
An integer vector describing which PCs to use.
}
  \item{dims}{
A integer vector giving the PCA dimensions to use.
}
  \item{title}{
A character string for the plot title.
}
  \item{plot}{
A character string indicating what plot to make.  Options are \code{c("BIC", "proj", "error")}; see \code{\link{Mclust}} for details.
}
  \item{use.sym}{
Logical; if true, the color scheme is changed to black and symbols are used for plotting.
}
  \item{\dots}{
Other parameters to be passed downstream.
}
}
\value{
The \code{Mclust} model is returned invisibly, and a plot is made.
}
\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\seealso{
\code{\link{Mclust}} for the underlying function.
}
\examples{
data(CuticleIR)
class <- classPCA(CuticleIR)
mclustSpectra(CuticleIR, class, title = "Cuticle IR", plot = "BIC")
mclustSpectra(CuticleIR, class, title = "Cuticle IR", plot = "proj")
mclustSpectra(CuticleIR, class, title = "Cuticle IR", plot = "error",
	truth = CuticleIR$groups)
}
\keyword{ multivariate }
\keyword{ cluster }

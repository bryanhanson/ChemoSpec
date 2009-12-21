\name{plotScree}
\Rdversion{1.1}
\alias{plotScree}

\title{
Scree Plot of PCA Results for a Spectra Object
}

\description{
Function to draw a scree plot illustrating the importance of the components in a PCA analysis of a \code{"Spectra"} object.
}

\usage{
plotScree(pca, title = "no title provided", ...)
}

\arguments{
  \item{pca}{
An object of class \code{\link{prcomp}}, modified to include a list element called \code{$method}, a character string describing the pre-processing carried out and the type of PCA performed (it appears on the plot).  This is automatically provided if \code{ChemoSpec} functions \code{\link{classPCA}} or \code{\link{robPCA}} were used to create \code{pca}.
}
  \item{title}{
A character string for the plot title.
}
  \item{\dots}{
Additional parameters to be passed to plotting functions.}
}
\value{
None.  Side effect is a plot.
}

\details{
Technically, if you add \code{$method} to the PCA results from other packages, this will plot a scree plot for any PCA results, not just those from \code{"Spectra"} objects.
}


\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
data(CuticleIR)
results <- classPCA(CuticleIR, choice = "noscale")
plotScree(results, title = "Cuticle IR Spectra")
}

\keyword{ multivariate }
\keyword{ hplot }
\name{plotScree}
\Rdversion{1.1}
\alias{plotScree}
\alias{plotScree2}

\title{
Scree Plots of PCA Results for a Spectra Object
}

\description{
Functions to draw a traditional scree plot or an alternative that is perhaps more useful.  These illustrate the importance of the components in a PCA analysis.}

\usage{
plotScree(pca, ...)
plotScree2(pca, ...)
}

\arguments{
  \item{pca}{
An object of class \code{\link{prcomp}}, modified to include a list element called \code{$method}, a character string describing the pre-processing carried out and the type of PCA performed (it appears on the plot).  This is automatically provided if \code{ChemoSpec} functions \code{\link{c_pcaSpectra}} or \code{\link{r_pcaSpectra}} were used to create \code{pca}.
}
  \item{\dots}{
Additional parameters to be passed to plotting functions.}
}
\value{
None.  Side effect is a plot.
}

\details{
If you add \code{$method} to the PCA results from other packages, this will plot a scree plot for any PCA results, not just those from \code{\link{Spectra}} objects.
}


\references{
The idea for the alternative style plot came from the NIR-Quimiometria blog by jrcuesta, at \url{http://nir-quimiometria.blogspot.com/2012/02/pca-for-nir-spectrapart-004-projections.html}

\url{https://github.com/bryanhanson/ChemoSpec}

}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
data(metMUD1)
results <- c_pcaSpectra(metMUD1)
plotScree(results, main = "metMUD1 NMR Data")
plotScree2(results, main = "metMUD1 NMR Data")
}

\keyword{ multivariate }
\keyword{ hplot }

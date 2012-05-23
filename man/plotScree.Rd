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
plotScree(pca, title = "no title provided", ...)
plotScree2(pca, title = "no title provided", ...)
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
If you add \code{$method} to the PCA results from other packages, this will plot a scree plot for any PCA results, not just those from \code{"Spectra"} objects.
}


\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
The idea for the alternative style plot came from the NIR-Quimiometria blog by jrcuesta, at \url{http://http://www.r-bloggers.com/pca-for-nir-spectra_part-004-projections/}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
data(SrE.IR)
results <- classPCA(SrE.IR, choice = "noscale")
plotScree(results, title = "Serenoa repens IR Spectra")
plotScree2(results, title = "Serenoa repens IR Spectra")
}

\keyword{ multivariate }
\keyword{ hplot }
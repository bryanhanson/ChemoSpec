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
An object of class \code{\link{prcomp}}, modified to include a list element called \code{$method}, a character string describing the pre-processing carried out and the type of PCA performed (it appears on the plot).  This is automatically provided if \code{ChemoSpec} functions \code{\link{classPCA}} or \code{\link{robPCA}} were used to create \code{pca}.
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
\url{https://github.com/bryanhanson/ChemoSpec}
The idea for the alternative style plot came from the NIR-Quimiometria blog by jrcuesta, at \url{http://http://www.r-bloggers.com/pca-for-nir-spectra_part-004-projections/}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
data(SrE.IR)
results <- classPCA(SrE.IR, choice = "noscale")
myt <- expression(bolditalic(Serenoa)~bolditalic(repens)~bold(IR~Spectra))
plotScree(results, main = myt)
plotScree2(results, main = myt)
}

\keyword{ multivariate }
\keyword{ hplot }

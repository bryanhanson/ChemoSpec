\name{plotLoadings}
\Rdversion{1.1}
\alias{plotLoadings}

\title{
Plot PCA Loadings for a Spectra Object
}
\description{
Creates a multi-panel plot of loadings along with a reference spectrum.
}

\usage{
plotLoadings(spectra, pca, loads = c(1), ref = 1, ...)
}

\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}.
}
  \item{pca}{
An object of class \code{\link{prcomp}}, modified to include a list element called \code{$method}, a character string describing the pre-processing carried out and the type of PCA performed (it appears on the plot).  This is automatically provided if \code{ChemoSpec} functions \code{\link{classPCA}} or \code{\link{robPCA}} were used to create \code{pca}.
}
  \item{loads}{
An integer vector giving the loadings to plot.  More than 3 loadings creates a useless plot using the default graphics window.
}
  \item{ref}{
An integer specifying the reference spectrum to plot, which appears at the bottom of the plot.
}
  \item{\dots}{
Additional parameters to be passed to plotting functions.
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
See \code{\link{plot2Loadings}} to plot two loadings against each other.
}
\examples{
data(SrE.IR)
results <- classPCA(SrE.IR, choice = "noscale")
myt <- expression(bolditalic(Serenoa)~bolditalic(repens)~bold(IR~Spectra))
plotLoadings(SrE.IR, results, main = myt,
	loads = 1:2, ref = 1)
}

\keyword{ multivariate }
\keyword{ hplot }

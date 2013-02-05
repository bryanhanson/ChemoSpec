\name{plotScores3D}
\Rdversion{1.1}
\alias{plotScores3D}
\title{
3D PCA Score Plot for a Spectra Object
}
\description{
Creates a basic 3D plot of PCA scores from the analysis of a \code{"Spectra"} object, color coded according the to scheme stored in the object.
}
\usage{
plotScores3D(spectra, pca, pcs = c(1:3),
	ellipse = TRUE, rob = FALSE, cl = 0.95, frac.pts.used = 0.80,
	title = "no title provided", view = list(y = 34, x = 10, z = 0),
	tol = 0.01, use.sym = FALSE, ...)
}
\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}.
}
  \item{pca}{
An object of class \code{\link{prcomp}}, modified to include a list element called \code{$method}, a character string describing the pre-processing carried out and the type of PCA performed (it appears on the plot).  This is automatically provided if \code{ChemoSpec} functions \code{\link{classPCA}} or \code{\link{robPCA}} were used to create \code{pca}.
}
  \item{pcs}{
A vector of three integers specifying the PCA scores to plot.
}
  \item{ellipse}{
Logical indicating if confidence ellipses should be drawn.
}
  \item{rob}{
Logical; if \code{ellipse = TRUE}, indicates that robust confidence ellipses should be drawn.  If \code{FALSE}, classical confidence ellipses are drawn.
}
  \item{cl}{
A number indicating the confidence interval for the ellipse.
}
  \item{frac.pts.used}{
If \code{ellipse = TRUE} and \code{rob = TRUE}, a number indicating the fraction of the data points to be considered "good" and thus used to compute the robust confidence ellipse.
}
  \item{title}{
A character string for the plot title.
}
  \item{view}{
A list of viewing transformations to be applied to the data.  May contain values for x, y and z axes; keep in mind that the order of the transformations is important.  For example, specifying \code{view = list(x = 45, y = 10)} produces a different view than \code{view = list(y = 10, x = 45)}.  The list may be as along as you like - the series of transformations representing an accumulation of tweaks to achieve the desired view.
}
  \item{tol}{Quantile to be used to label extreme data points.  Currently not used - need to fix the code!}
  \item{use.sym}{logical; if true, the color scheme is change to black and symbols are used for plotting.}
  \item{\dots}{Other parameters to be passed downstream.}

}
\value{
None.  Side effect is a plot.
}

\seealso{
For a 2D plot of the scores, see \code{\link{plotScores}}.  For more sophisticated 3D plots, use \code{\link{plotScoresRGL}}.
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
plotScores3D(CuticleIR, results, title = "Cuticle IR Spectra")
}
\keyword{ multivariate }
\keyword{ hplot }

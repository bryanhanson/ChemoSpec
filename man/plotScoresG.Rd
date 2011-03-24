\name{plotScoresG}
\Rdversion{1.1}
\alias{plotScoresG}
\title{
Plot PCA Scores of a Spectra Object using GGobi
}
\description{
Plots the specified PCs from a PCA analysis of a \code{"Spectra"} object using \code{GGobi}.
}
\usage{
plotScoresG(spectra, pca, pcs = c(1:3), scheme = "primary")
}
\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}.
}
  \item{pca}{
An object of class \code{\link{prcomp}}.
}
  \item{pcs}{
A vector of integers specifying the scores to plot.  \code{GGobi} handles multivariate data, and so it is not necessary to limit the analysis to 3 dimensions.
}
  \item{scheme}{
The color scheme to use, one of \code{c("primary", "pastel")}.  For details on color options, see \code{\link{colorSymbol}}.
}
}
\value{
None.  Side effect is a plot in an X11 window created by \code{GGobi}.  The mapping of colors from the \code{Spectra} object to the \code{GGobi} color schemes is printed at the console, since the original color choices may not be acceptable to \code{GGobi}.
}

\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}\cr
\url{http://www.ggobi.org/}\cr
D. Cook & D. F. Swayne, \emph{Interactive and Dynamic Graphics for Data Analysis with R and GGobi}, Springer 2007.
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
For plotting only 3 scores with rudimentary control over the view, see \code{\link{plotScores3D}}.  For an interactive plot of exactly 3 scores, see \code{\link{plotScoresRGL}}.  For plotting 2 scores, see \code{\link{plotScores}}.  For details on color options, see \code{\link{colorSymbol}}.
}
\examples{
data(CuticleIR)
require(rggobi)
results <- classPCA(CuticleIR, choice = "noscale")
\dontrun{plotScoresG(CuticleIR, results)}
# GGobi runs interactively using X11
}
\keyword{ multivariate }
\keyword{ hplot }

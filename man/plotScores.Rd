\name{plotScores}
\Rdversion{1.1}
\alias{plotScores}

\title{
Plot PCA Scores of a Spectra Object
}
\description{
Plots the requested PCA scores using the color scheme derived from a \code{"Spectra"} object.  Options are provided to add confidence ellipses for each group in the object.  The ellipses may be robust or classical.  Option to label the extreme points provided.
}
\usage{
plotScores(spectra, pca, title = "no title provided",
	pcs = c(1, 2), ellipse = "none", tol = "none",
	use.sym = FALSE, ...)
}

\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}.
}
  \item{pca}{
An object of class \code{\link{prcomp}}, modified to include a list element called \code{$method}, a character string describing the pre-processing carried out and the type of PCA performed (it appears on the plot).  This is automatically provided if \code{ChemoSpec} functions \code{\link{classPCA}} or \code{\link{robPCA}} were used to create \code{pca}.
}
  \item{title}{
A character string for the plot title.
}
  \item{pcs}{
A vector of two integers specifying the PCA scores to plot.
}
  \item{ellipse}{
A character vector specifying the type of ellipses to be plotted.  One of \code{c("both", "none", "cls", "rob")}.  \code{cls} specifies classical confidence ellipses, \code{rob} specifies robust confidence ellipses.
}
  \item{tol}{
A number describing the fraction of points to be labeled.  \code{tol = 1.0} labels all the points; \code{tol = 0.05} labels the most extreme 5 percent.
}
  \item{use.sym}{
A logical; if true, the color scheme is set to black and the points plotted with symbols.
}
  \item{\dots}{
Additional parameters to be passed to the plotting functions.
}
}
\value{
None.  Side effect is a plot.
}

\section{Calls}{\code{\link{plotScoresDecoration}}, \code{\link{plotScoresCor}}, \code{\link{chkSpectra}}
}

\seealso{
For other ways of displaying the results, \code{\link{plotScree}}, \code{\link{plotLoadings}}, \code{\link{plot2Loadings}}. For a 3D plot of the scores, see \code{\link{plotScores3D}}.  

}

\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
data(SrE.IR)
results <- classPCA(SrE.IR, choice = "noscale")
plotScores(SrE.IR, title = "Serenoa repens IR Spectra", results,
	pcs = c(1,2), ellipse = "rob", tol = 0.05)
}

\keyword{ multivariate }
\keyword{ robust }
\keyword{ hplot }

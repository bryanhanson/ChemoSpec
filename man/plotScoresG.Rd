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
The color scheme to use, one of \code{c("primary", "pastel")}.  See details for more on color choices.
}
}
\details{
In general, users many specify any valid R color for use in \code{ChemoSpec}.  However, the color schemes available to \code{GGobi} are limited.  If you want all your graphics to use the same color scheme, you need to specify a color scheme useable with \code{GGobi} at the time the \code{"Spectra"} object is created using the function \code{\link{getManyCsv}}.  Two particular color schemes have been hard-coded into the function described here; the choices are:

primary: \code{c("red3", "dodgerblue4", "forestgreen", "purple4", "orangered", "yellow",
	"orangered4", "violetred2")}

pastel: \code{c("seagreen", "brown2", "skyblue2", "hotpink3", "chartreuse3",
	"darkgoldenrod2", "lightsalmon3", "gray48")}

These are very close matches to the color schemes used in \code{GGobi}, which in turn come from \code{RColorBrewer}.  If you want to see what these colors look like, use \code{display.brewer.pal(8, "Set1)} or \code{display.brewer.pal(8, "Set2)}.

The behavior of this function is to first check to see if a \code{GGobi}-compatible color scheme already exists in \code{spectra}.  If it does, it is re-used with the same mapping of colors to groups as originally specified and stored in \code{spectra}.  If the color scheme is not \code{GGobi}-compatible, color assignments for \code{GGobi} window are generated and the mapping of old colors to new colors is reported at the console.

Finally, a difficulty to bear in mind is that R plots are generally on a white background, so pale colors should be avoided, while \code{GGobi} plots on a black background (interactively), so dark colors should be avoided!  Plus, \code{ggplot2} plots on a grey background by default!  So it is not necessarily possible to specify a single color scheme that works with all possible plotting systems in R.


}
\value{
None.  Side effect is a plot in an X11 window created by \code{GGobi}.  The mapping of colors from the \code{Spectra} object to the \code{GGobi} color schemes is printed at the console, since the original color choices may not be acceptable to \code{GGobi}.
}
\section{Calls}{\code{\link{chkSpectra}}; Requires that \code{GGobi} be installed and X11 running.}
\section{Called by}{Top level function, called by user.}

\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}\cr
\url{http://www.ggobi.org/}\cr
D. Cook & D. F. Swayne, \emph{Interactive and Dynamic Graphics for Data Analysis with R and GGobi}, Springer 2007.
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
For plotting only 3 scores with rudimentary control over the view, see \code{\link{plotScores3D}}.  For plotting 2 scores, see \code{\link{plotScores}}.
}
\examples{
data(CuticleIR)
require(ggobi)
results <- classPCA(CuticleIR, choice = "noscale")
\dontrun{plotScoresG(CuticleIR, results)}
# GGobi runs interactively using X11
}
\keyword{ multivariate }

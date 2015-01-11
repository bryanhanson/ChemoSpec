\name{plotScoresRGL}
\alias{plotScoresRGL}
\title{
Interactive 3D Score Plot of a Spectra Object
}
\description{
This function uses the \code{\link{rgl}} package to create an interactive plot of PCA scores derived from a \code{"Spectra"} object.  A title and legend can be added if desired.  Classical or robust confidence ellipses may be added if desired.
}
\usage{
plotScoresRGL(spectra, pca, pcs = c(1:3), ellipse = TRUE,
	rob = FALSE, cl = 0.95, frac.pts.used = 0.8,
	title = NULL, t.pos = NULL, leg.pos = NULL, lab.opts = FALSE,
	tol = 0.01, use.sym = FALSE, ...)
}
\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}.
}
  \item{pca}{
An object of class \code{\link{prcomp}}.
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
  \item{t.pos}{
A character selection from \code{LETTERS[1:8]} ( = A through H) indicating the desired location for the title.
}
  \item{leg.pos}{
A character selection from \code{LETTERS[1:8]} ( = A through H) indicating the desired location for the legend.
}
  \item{lab.opts}{
A logical indicating whether or not to display the locations where the title and legend can be placed.  These locations are the corners of a cube surrounding the data.
}
  \item{tol}{Quantile to be used to label extreme data points.}
  \item{use.sym}{logical; if true, the color scheme is changed to black and symbols are used for plotting.}

  \item{\dots}{
Additional parameters to pass downstream, generally to the plotting routines.
}
}
\details{
If you intend to make a hard copy of your plot, use \code{lab.opts = TRUE} until you have found a good view of your data.  Then note corners of the cube where the title and legend won't interfere with viewing the data, and use these as arguments for \code{t.pos} and \code{leg.pos}, and add \code{title}.  Adjust as necessary, then turn off label display using \code{lab.opts = FALSE}.  Back at the console, use \code{> rgl.snapshot("file_name.png")} to create the hardcopy.
}
\value{
None.  Side effect is a plot
}
\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
Other functions in \code{ChemoSpec} that plot PCA scores are: \code{\link{plotScores}} (2D version), and \code{\link{plotScores3D}} (uses \code{lattice} graphics).
}
\examples{
data(CuticleIR)
results <- classPCA(CuticleIR, choice = "autoscale")
\dontrun{
plotScoresRGL(CuticleIR, results, title = "Cuticle IR Spectra",
leg.pos = "A", t.pos = "B")
}
}

\keyword{ multivariate }
\keyword{ hplot }
\keyword{ dynamic }

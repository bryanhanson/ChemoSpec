\name{plotScoresRGL}
\alias{plotScoresRGL}
\title{
Interactive 3D Score Plot of a Spectra Object
}
\description{
This function uses the \code{\link{rgl}} package to create an interactive plot of PCA scores derived from a \code{"Spectra"} object.  A title and legend can be added if desired.
}
\usage{
plotScoresRGL(spectra, pca, pcs = c(1:3), title = NULL,
	t.pos = NULL, leg.pos = NULL, lab.opts = FALSE, ...)
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
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
Other functions in \code{ChemoSpec} that plot PCA scores are: \code{\link{plotScores}} (2D version), \code{\link{plotScores3D}} (uses \code{lattice} graphics) and \code{\link{plotScoresG}} which uses \code{ggobi} and \code{rggobi}.
}
\examples{
data(CuticleIR)
results <- classPCA(CuticleIR, choice = "autoscale")
plotScoresRGL(CuticleIR, results, title = "Cuticle IR Spectra", leg.pos = "A", t.pos = "B")
}

\keyword{ multivariate }

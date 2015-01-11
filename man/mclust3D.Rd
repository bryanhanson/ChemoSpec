\name{mclust3D}
\alias{mclust3D}
\title{
mclust Analysis in 3D
}
\description{
This function conducts an mclust analysis of the data provided, and plots the points in 3D using rgl graphics.  An option is provided for displaying either classical or robust confidence ellipses.
}
\usage{
mclust3D(data, ellipse = TRUE, rob = FALSE, cl = 0.95,
	frac.pts.used = 0.8, truth = NULL,
	title = "no title provided", t.pos = NULL,
	lab.opts = FALSE, use.sym = FALSE, ...)
}
\arguments{
  \item{data}{
A matrix of 3 columns (corresponding to x, y, z) and samples in rows.
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
  \item{truth}{
A character vector indicating the known group membership for reach row of the PC scores.  Generally this would be \code{spectra$groups}.
}
  \item{title}{
A character string for the plot title.
}
  \item{t.pos}{
A character selection from \code{LETTERS[1:8]} ( = A through H) indicating the desired location for the title.
}
  \item{lab.opts}{
A logical indicating whether or not to display the locations where the title and legend can be placed.  These locations are the corners of a cube surrounding the data.
}
  \item{use.sym}{
logical; if true, the color scheme is changed to black and symbols are used for plotting.
}
  \item{\dots}{
Other parameters to be passed downstream.
}
}
\details{
If you intend to make a hard copy of your plot, use \code{lab.opts = TRUE} until you have found a good view of your data.  Then note corners of the cube where the title won't interfere with viewing the data, and use this for \code{t.pos}, and add \code{title}.  Adjust as necessary, then turn off label display using \code{lab.opts = FALSE}.  Back at the console, use \code{> rgl.snapshot("file_name.png")} to create the hardcopy.
}
\value{
The mclust model is returned invisibly, and a plot is produced.
}
\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
set.seed(666)
x <- c(rnorm(10, 3, 0.5), rnorm(10, -1, 0.5))
y <- c(rnorm(10, 1, 1), rnorm(10, -4, 0.5))
z <- c(rnorm(10, -2, 0.5), rnorm(10, 3, 0.5))
x[15] <- y[15] <- z[15] <- 4 # screw up one point
my.truth <- c(rep("Z", 10), rep("Q", 10))
\dontrun{
mclust3D(cbind(x, y, z), title = "mclust3D demo",
	t.pos = "G", truth = my.truth)
}
}
\keyword{ multivariate }
\keyword{ cluster }

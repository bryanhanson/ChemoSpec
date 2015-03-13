\name{mclust3dSpectra}
\alias{mclust3dSpectra}
\title{
mclust Analysis of a Spectra Object in 3D
}
\description{
This function conducts an mclust analysis of the PCA results of a \code{\link{Spectra}} object and displays the results in 3D.  Classical or robust confidence ellipses can be added if desired. Improperly classified data points can be marked.  rgl graphics are employed.
}
\usage{
mclust3dSpectra(spectra, pca, pcs = c(1:3),
	ellipse = TRUE, rob = FALSE, cl = 0.95, frac.pts.used = 0.8,
	truth = NULL, title = "no title provided",
	t.pos = NULL, lab.opts = FALSE, use.sym = FALSE, ...)
}
\arguments{
  \item{spectra}{
An object of S3 class \code{\link{Spectra}}.
}
  \item{pca}{
An object of class \code{\link{prcomp}}.
}
  \item{pcs}{
An integer vector describing which PCs to use.
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
Logical; if true, the color scheme is changed to black and symbols are used for plotting.
}
  \item{\dots}{
Other parameters to be passed downstream.
}
}
\details{
If you intend to make a hard copy of your plot, use \code{lab.opts = TRUE} until you have found a good view of your data.  Then note corners of the cube where the title won't interfere with viewing the data, and use this for \code{t.pos}, and add \code{title}.  Adjust as necessary, then turn off label display using \code{lab.opts = FALSE}.  Back at the console, use \code{> rgl.snapshot("file_name.png")} to create the hardcopy.

Note that the confidence ellipses computed here are generated independently of the \code{Mclust} results - they do not correspond to the ellipses seen in 2D plots from \code{Mclust}.
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
\dontrun{
data(metMUD1)
class <- c_pcaSpectra(metMUD1)
mclust3dSpectra(metMUD1, class, title = "mclust3dSpectra demo",
	lab.opts = FALSE, t.pos = "A")
}
}
\keyword{ multivariate }
\keyword{ cluster }

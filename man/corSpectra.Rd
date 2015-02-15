\name{corSpectra}
\alias{corSpectra}
\alias{covSpectra}

\title{
Tools for STOCSY Analysis of a Spectra Object
}

\description{
These functions provide tools for STOSCY analysis of a \code{\link{Spectra}} object, using the method developed by Nicholson.  STOCSY is Statistical Total Correlation Spectroscopy.  Briefly, the correlation matrix of an NMR data set of \code{n} samples and \code{p} frequencies is computed (the matrix dimensions are \code{p x p}).  Peaks arising from the same compound are intrinsically positively correlated.  This is much like a 1D or 2D TOCSY NMR spectrum. However, peaks that are correlated, positively or negatively, due to metabolic processes, will also appear in the STOCSY plot. \code{corSpectra} computes the correlation and covariance matrices, and can display the correlation matrix in several formats.  Detailed inspection/interpretation of this plot is tedious, and producing it can be slow for large data sets, so it's most useful as an overview.  \code{covSpectra} will display a single frequency (i.e. chemical shift) from the covariance matrix, but color it according to the correlation matrix values.  This is point where detailed interpretation is done.  See the example.
}

\usage{
corSpectra(spectra, plot = TRUE, limX = NULL, limY = NULL, nticks = 10,
levels = NULL, pmode = "contour", C = NULL, V = NULL, ...)

covSpectra(spectra, freq = spectra$freq[1], C = NULL, V = NULL, ...)
}

\arguments{
  \item{spectra}{
An object of S3 class \code{\link{Spectra}}.
}
  \item{plot}{
Logical.  Should a plot be made?  Applies to \code{corSpectra} only.
}
  \item{limX}{
Numeric vector of length 2.  The x limits.  Applies to \code{corSpectra} only.
}
  \item{limY}{
Numeric vector of length 2.  The y limits.  Applies to \code{corSpectra} only.
}
  \item{nticks}{
Integer.  The number of ticks to be drawn.  Applies to \code{corSpectra} only.
}
  \item{levels}{
Numeric.  A vector of values at which to draw the contours or levels.  Applies to \code{corSpectra} only.  The default is to use \code{\link{chooseLvls}} to compute 5 evenly spaced levels.  For most data sets this should only be considered a starting point. A histogram of the correlation matrix can be very helpful in choosing levels.
}
  \item{pmode}{
Character.  The plot mode.   Applies to \code{corSpectra} only. One of \code{c("contour", "image", "contourplot", "levelplot", "rgl", "exCon")}.  The last two are interactive.  See Details.
}
  \item{C}{
Matrix.  Optional. A precomputed correlation matrix.
}
  \item{V}{
Matrix.  Optional. A precomputed covariance matrix.
}
  \item{freq}{
Numeric.  The desired frequency to be plotted.   Applies to \code{covSpectra} only.
}

  \item{\dots}{
Other parameters to be passed to the plotting functions.
}
}

\value{
A list giving the covariance and correlation matrices.  A plot will also be made if \code{plot = TRUE} for \code{corSpectra}.
}

\references{
O. Cloarec et. al. Analytical Chemistry vol. 77 pgs. 1282-1289 (2005).

\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}


\seealso{
\code{\link{chooseLvls}} to select levels automatically.

\code{\link{getMaxCovByFreq}} is a utility to sort the covariance matrix by the maximum covariance at a given frequency.  This can be used to identify the strongest interactions.
}

\section{Warning}{
For \code{corSpectra}, the labeling of the axis will be wrong if there is a gap in the data.
}

\details{
If \code{spectra$freq} is in decreasing order, it and \code{spectra$data} are silently re-ordered to be increasing before plotting.

The calculation of the correlation and covariance matrices may take quite some time for large data sets.  It is possible to pre-compute these and pass them into the functions to save time and avoid repetition.

Plotting in \code{corSpectra} can be extremely slow for large data sets.  The base graphics options (\code{pmode = "contour"} or \code{"image"}) are much faster than the \code{lattice} options.  These plots are probably best for an overall sense of the data and for publication rather than detailed interpretation. If using \code{pmode = "contour"} drawing fewer contours is of course faster for both drawing and computation of the contours.  Note too that contour style plots have \code{n} colors for \code{n} contour levels but image style plots have \code{n-1} colors for \code{n} levels.

The color scale for the plots is blue/low correlation to red/high correlation, anchored at a shade of green for zero correlation.  The example shows how to see the color scale.  

For \code{covSpectra}, the x and y limits can be set simply by passing \code{xlim} and \code{ylim} via the \ldots.
}

\examples{
# The color scale used:
# cscale <- c(rev(rainbow(4, start = 0.45, end = 0.66)), rev(rainbow(5, start = 0.0, end = 0.25)))
# pie(rep(1, 9), col = cscale)
#
# This data set is a mixture of compounds, some of
# which have correlated concentrations, both
# positively and negatively.  The contour plot shows
# correlations w/i a spin system and among compounds.
# The 2nd plot shows one particular frequency.
# For more info about the data set see ?MUD
#
data(metMUD2)
lvs <- c(-0.99, -0.95, -0.9, 0.9, 0.95, 0.99)
lim <- c(0.6, 4.3)
res <- corSpectra(metMUD2, levels = lvs, limX = lim, limY = lim, main = "metMUD2 NMR Data")
jnk <- covSpectra(metMUD2, freq = 1.0, C = res[[2]], V = res[[1]])
}

\keyword{ hplot }

#'
#' DEFUNCT>> 3D PCA Score Plot for a Spectra Object << DEFUNCT
#'
#' Creates a basic 3D plot of PCA scores from the analysis of a
#' \code{\link{Spectra}} object, color coded according the to scheme stored in
#' the object.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param pca An object of class \code{\link{prcomp}}, modified to include a
#' list element called \code{$method}, a character string describing the
#' pre-processing carried out and the type of PCA performed (it appears on the
#' plot).  This is automatically provided if \code{ChemoSpec} functions
#' \code{\link{c_pcaSpectra}} or \code{\link{r_pcaSpectra}} were used to create
#' \code{pca}.
#'
#' @param pcs A vector of three integers specifying the PCA scores to plot.
#'
#' @param ellipse Logical indicating if confidence ellipses should be drawn.
#'
#' @param rob Logical; if \code{ellipse = TRUE}, indicates that robust
#' confidence ellipses should be drawn.  If \code{FALSE}, classical confidence
#' ellipses are drawn.
#'
#' @param cl A number indicating the confidence interval for the ellipse.
#'
#' @param frac.pts.used If \code{ellipse = TRUE} and \code{rob = TRUE}, a
#' number indicating the fraction of the data points to be considered "good"
#' and thus used to compute the robust confidence ellipse.
#'
#' @param view A list of viewing transformations to be applied to the data.
#' May contain values for x, y and z axes; keep in mind that the order of the
#' transformations is important.  For example, specifying \code{view = list(x =
#' 45, y = 10)} produces a different view than \code{view = list(y = 10, x =
#' 45)}.  The list may be as along as you like - the series of transformations
#' representing an accumulation of tweaks to achieve the desired view.
#'
#' @param tol Quantile to be used to label extreme data points.  Currently not
#' used - need to fix the code!
#'
#' @param use.sym logical; if true, the color scheme is change to black and
#' symbols are used for plotting.
#'
#' @param \dots Other parameters to be passed downstream.
#'
#' @return None.  Side effect is a plot.
#'
#' @template authors-BH
#'
#' @seealso For a 2D plot of the scores, see \code{\link{plotScores}}.  For
#' interactive 3D plots, use \code{plotScoresRGL}.  Additional documentation
#' at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @keywords multivariate hplot
#' @export plotScores3D
#'
#' @examples
#' \dontrun{
#' data(metMUD1)
#' pca <- c_pcaSpectra(metMUD1, choice = "noscale")
#' plotScores3D(metMUD1, pca, main = "metMUD1 NMR Spectra")
#' }
#'
plotScores3D <- function(spectra, pca, pcs = c(1:3), ellipse = TRUE, rob = FALSE,
                         cl = 0.95, frac.pts.used = 0.8,
                         view = list(y = 34, x = 10, z = 0), tol = 0.01, use.sym = FALSE, ...) {
  .Defunct("plot3dScores", "ChemoSpec", "plotScores3D has been removed from ChemoSpec.  Please use plot3dScores instead.")

}

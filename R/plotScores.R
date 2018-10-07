#'
#'
#'
#' Plot PCA Scores of a Spectra Object
#' 
#' Plots the requested PCA scores using the color scheme derived from a
#' \code{\link{Spectra}} object.  Options are provided to add confidence
#' ellipses for each group in the object.  The ellipses may be robust or
#' classical.  Option to label the extreme points provided.
#' 
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
#' @param pcs A vector of two integers specifying the PCA scores to plot.
#'
#' @param ellipse A character vector specifying the type of ellipses to be
#' plotted.  One of \code{c("both", "none", "cls", "rob")}.  \code{cls}
#' specifies classical confidence ellipses, \code{rob} specifies robust
#' confidence ellipses.  An ellipse is drawn for each group in 
#' \code{spectra$groups}.
#'
#' @param tol A number describing the fraction of points to be labeled.
#' \code{tol = 1.0} labels all the points; \code{tol = 0.05} labels the most
#' extreme 5 percent.
#'
#' @param use.sym A logical; if true, the color scheme is set to black and the
#' points plotted with symbols.
#'
#' @param leg.loc Character; if \code{"none"} no legend will be drawn.
#' Otherwise, any string acceptable to \code{\link{legend}}.
#'
#' @param \dots Additional parameters to be passed to the plotting functions.
#'
#' @return None.  Side effect is a plot.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso For other ways of displaying the results, \code{\link{plotScree}},
#' \code{\link{plotLoadings}}, \code{\link{plot2Loadings}}. For a 3D plot of
#' the scores, see \code{\link{plotScores3D}}, or \code{\link{plotScoresRGL}}
#' for an interactive version.
#'
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#'
#' @keywords multivariate robust hplot
#'
#' @examples
#' 
#' data(metMUD1)
#' pca <- c_pcaSpectra(metMUD1)
#' plotScores(metMUD1, pca, main = "metMUD1 NMR Data",
#' 	 pcs = c(1,2), ellipse = "cls", tol = 0.05)
#' 
#' @export plotScores
#'
plotScores <- function(spectra, pca,
	pcs = c(1,2), ellipse = "none", tol = "none",
	use.sym = FALSE, leg.loc = "topright", ...) {

	.plotScores(spectra, pca,
	pcs = c(1,2), ellipse = ellipse, tol = tol,
	use.sym = use.sym, leg.loc = leg.loc, ...)

}


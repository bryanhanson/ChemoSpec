#'
#'
#' Robust PCA of a Spectra Object
#' 
#' A wrapper which carries out robust PCA analysis on a \code{\link{Spectra}}
#' object.  The data are row- and column-centered, and the user can select
#' various options for scaling.
#' 
#' 
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param choice A character vector describing the type of scaling to be
#' carried out.  One of \code{c("noscale", "mad")}.
#'
#' @return An object of classes \code{conPCA} and \code{princomp} (see
#' \code{\link{q2rPCA}}).  It includes a list element called \code{$method}, a
#' character string describing the pre-processing carried out and the type of
#' PCA performed (it appears on plots which you might make).
#'
#' @seealso See \code{\link[pcaPP]{PCAgrid}} on which this function is based.
#' For the classical version, see \code{\link{c_pcaSpectra}}.
#' 
#' For displaying the results, \code{\link{plotScree}}, 
#' \code{\link{plotScores}}, \code{\link{plotLoadings}},
#' \code{\link{plot2Loadings}}, \code{\link{sPlotSpectra}},
#' \code{\link{plotScores3D}}, \code{\link{plotScoresRGL}}.
#'
#' @references K. Varmuza and P. Filzmoser \emph{Introduction to Multivariate
#' Statistical Analysis in Chemometrics}, CRC Press, 2009.
#' 
#' \url{https://github.com/bryanhanson/ChemoSpec}\cr
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords multivariate robust
#'
#' @examples
#' 
#' data(metMUD1)
#' pca <- r_pcaSpectra(metMUD1)
#' plotScores(metMUD1, pca, main = "metMUD1 NMR Data",
#'   pcs = c(1,2), ellipse = "cls", tol = 0.05)
#' 
#' @export r_pcaSpectra
#'
# @importFrom pcaPP PCAgrid
#'
r_pcaSpectra <- function(spectra, choice = "noscale") {

	if (!requireNamespace("pcaPP", quietly = TRUE)) {
		stop("You need to install package pcaPP to use this function")
	}
	
	if (missing(spectra)) stop("No spectral data set provided")
	chkSpectra(spectra)
	
	choices <- c("noscale", "mad") # trap for invalid scaling method
	check <- choice %in% choices
	if (!check) stop("The choice of scaling parameter for robust PCA was invalid")

	# Note: PCAgrid produces an object of class princomp, not prcomp
	# so there must be some conversion to match class prcomp
	
	note <- choice
	if (choice == "noscale") choice <- NULL
	pca <- pcaPP::PCAgrid(spectra$data, k = 10, scale = choice, scores = TRUE)
	pca$method <- paste("l1median/", note, "/", "robust", sep = "")
	pca <- r2qPCA(pca) # convert classes
	}


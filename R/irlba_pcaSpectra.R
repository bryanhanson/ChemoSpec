#' IRLBA PCA of Spectra Objects
#' 
#' A wrapper which carries out IRLBA PCA analysis on a
#' \code{\link{Spectra}} object.  The user can select various options for
#' scaling.  There is no normalization by rows - do this manually using
#' \code{\link{normSpectra}}. The data can be supplied already centered
#' if desired.
#' 
#' The scale choice \code{autoscale} scales the columns by their standard
#' deviation.  \code{Pareto} scales by the square root of the standard
#' deviation.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#' 
#' @param choice A character string indicating the choice of scaling.  One of
#' \code{c("noscale"}, \code{"autoscale"}, \code{"Pareto")}. \code{"autoscale"}
#' is called "standard normal variate" or "correlation matrix PCA" in some literature.
#' 
#' @param n Integer. The number of components desired.
#' 
#' @param center Logical.  Should the data be centered?  Data must be centered
#' for PCA, either before arriving here or via this argument.
#' 
#' @param ... Other parameters to be passed to \code{\link[irlba]{irlba}}.
#' 
#' @return A modified object of class \code{prcomp} and \code{computed_via_irlba},
#' which includes a list element called \code{$method}, a character string describing the
#' pre-processing carried out and the type of PCA performed (used to annotate
#' plots).
#' 
#' @author Bryan A. Hanson, DePauw University.
#' 
#' @seealso \code{\link[irlba]{prcomp_irlba}} for the underlying function,
#' \code{\link{c_pcaSpectra}} for classical PCA calculations,
#' \code{\link{r_pcaSpectra}} for robust PCA calculations,
#' \code{\link{s_pcaSpectra}} for sparse PCA calculations.
#' Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#' 
#' For displaying the results, \code{\link{plotScree}},
#' \code{\link{plotScores}}, \code{\link{plotLoadings}},
#' \code{\link{plot2Loadings}}, \code{\link{sPlotSpectra}},
#' \code{\link{plotScores3D}}, \code{\link{plotScoresRGL}}.
#' 
#' @references J. Baglama and L. Reichel, "Augmented Implicitly Restarted Lanczos
#' Bidiagonalization Methods"  \emph{SIAM J. Sci. Comput.} (2005). 
#' 
#' @keywords multivariate
#' 
#' @examples
#' 
#' data(SrE.NMR)
#' pca <- irlba_pcaSpectra(SrE.NMR)
#' plotScree(pca)
#' plotScores(SrE.NMR, pca, main = "SrE NMR Data",
#' 	 pcs = c(1,2), ellipse = "cls", tol = 0.05)
#' plotLoadings(SrE.NMR, pca, main = "SrE NMR Data",
#' 	 loads = 1:2, ref = 1)
#' 
#' @export irlba_pcaSpectra
#' 
#' @importFrom stats sd
#' 
irlba_pcaSpectra <- function(spectra, choice = "noscale", n = 3, center = TRUE, ...) {

	if (!requireNamespace("irlba", quietly = TRUE)) {
		stop("You need to install package irlba to use this function")
	}

	.chkArgs(mode = 11L)
	chkSpectra(spectra)
	
	choices <- c("noscale", "autoscale", "Pareto") # trap for invalid scaling method
	check <- choice %in% choices
	if (!check) stop("The choice of scaling parameter was invalid")

	# irlba::prcomp_irlba does its own scaling, so we must plan for that
	
	if (choice == "noscale") {
		X <- scale(spectra$data, center = center, scale = FALSE)
	}

	if (choice == "Pareto") {
		col.sd <- apply(spectra$data, 2, sd)
		X <- scale(spectra$data, center = center, scale = col.sd^0.5)
	}

	if (choice == "autoscale") {
		col.sd <- apply(spectra$data, 2, sd)
		X <- scale(spectra$data, center = center, scale = col.sd)
	}
	
	pca <- irlba::prcomp_irlba(x = X, n = n, center = FALSE, scale. = FALSE, ...)

	# Modify the class
	pca$method <- paste("centered/", choice, "/", "irlba", sep = "")
	class(pca) <- c("computed_via_irlba", "prcomp")
	pca
	}


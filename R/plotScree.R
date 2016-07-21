#'
#'
#' Scree Plots of PCA Results for a Spectra Object
#' 
#' Functions to draw a traditional scree plot or an alternative that is perhaps
#' more useful.  These illustrate the importance of the components in a PCA
#' analysis.
#' 
#' If you add \code{$method} to the PCA results from other packages, this will
#' plot a scree plot for any PCA results, not just those from
#' \code{\link{Spectra}} objects.
#'
#' @param pca An object of class \code{\link{prcomp}}, modified to include a
#' list element called \code{$method}, a character string describing the
#' pre-processing carried out and the type of PCA performed (it appears on the
#' plot).  This is automatically provided if \code{ChemoSpec} functions
#' \code{\link{c_pcaSpectra}} or \code{\link{r_pcaSpectra}} were used to create
#' \code{pca}.
#'
#' @param \dots Additional parameters to be passed to plotting functions.
#'
#' @return None.  Side effect is a plot.
#'
#' @aliases plotScree plotScree2
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @references The idea for the alternative style plot came from the NIR-Quimiometria
#'  blog by jrcuesta, at \url{https://nir-quimiometria.blogspot.com/2012/02/pca-for-nir-spectrapart-004-projections.html}
#' 
#' \url{https://github.com/bryanhanson/ChemoSpec}
#'
#' @keywords multivariate hplot
#'
#' @examples
#' 
#' data(metMUD1)
#' pca <- c_pcaSpectra(metMUD1)
#' plotScree(pca, main = "metMUD1 NMR Data")
#' plotScree2(pca, main = "metMUD1 NMR Data")
#' 
#' @export plotScree plotScree2
#'
#' @importFrom graphics plot axis points abline legend
#'
#' @describeIn plotScree Traditional scree plot
plotScree <- function(pca,  ...) {

# Function to do the scree plot
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, June 2008
# pca must be of class prcomp
	
	if (missing(pca)) stop("No PCA results provided")
	if (!("princomp" %in% class(pca) || "prcomp" %in% class(pca))) stop("Your pca results look corrupt!")
	
	eigensum <- sum(pca$sdev*pca$sdev)
	variance <- 100*(pca$sdev*pca$sdev/eigensum)
	cumvariance <- variance  # temporary definition as a vector of proper length
	for (n in c(1:length(variance))) {cumvariance[n] <- sum(variance[1:n])}

	ncp <- length(variance)
	if (ncp > 10) ncp <- 10
	
	plot(c(1:ncp), variance[1:ncp], type = "l", col = "red", xlab = "factor", ylab = "percent", ylim = c(0,100), ...)
	axis(1, at = c(1:ncp), labels = TRUE)
	points(c(1:ncp), cumvariance[1:ncp], type="l", col="blue")
	
	abline(v = c(1:ncp), h = c(0,10,20,30,40,50,60,70,80,90,100), col = "lightgray")
	abline(h = 95, lty = "dashed")
	legend("bottomleft", y = NULL, pca$method, bty = "n", cex = 0.75)
	legend("topright", y=NULL, "cumulative percent", lty = 1, bty = "n", inset = c(0, 0.40), col = "blue", cex = 0.75)
	legend("topright",y = NULL, " individual percent", lty = 1, bty = "n", inset = c(0, 0.50), col = "red", cex = 0.75)
	
	}


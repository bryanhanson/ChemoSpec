#'
#'
#'
#' Decorate PCA Score Plot of a Spectra Object
#' 
#' Utility function to carry out misc. labeling functions on the PCA score plot
#' of a \code{\link{Spectra}} object.  An internal function, not generally
#' called by the user.
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
#' @param tol A number describing the fraction of points to be labeled.
#' \code{tol = 1.0} labels all the points; \code{tol = 0.05} labels the most
#' extreme 5 percent.
#'
#' @return None.  The score plot is decorated.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#'
#' @keywords utilities
#'
#' @export plotScoresDecoration
#'
#' @importFrom graphics title legend
#'
plotScoresDecoration <- function(spectra, pca, pcs = c(1,2), tol = "none") {
	
# Function to do some plot annotations on the PCA score plot
# These tasks are done in all 4 cases in plotScores, so keeping it here makes
# maintaining & changing code easier.
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Aug 2009

	eigensum <- sum(pca$sdev*pca$sdev) # prepare axis labels
	variance <- 100*(pca$sdev*pca$sdev/eigensum)
	txt.x <- paste("PC", pcs[1], " score (", format(variance[pcs[1]], digits=2), "%", ")", sep = "")
	txt.y <- paste("PC", pcs[2], " score (", format(variance[pcs[2]], digits=2), "%", ")", sep = "")

	p <- pca$x[,pcs] # get info needed to label the points by sample names
	pl <- spectra$names
	
	lt <- levels(spectra$groups) # get legend stuff ready
	lc <- lt # intialize to correct length
	for (z in 1:length(lt)) {
		i <- match(lt[z], spectra$groups)
		lc[z] <- spectra$colors[i]
		}

	# now, actually label the plot
	
#	rug(pca$x[,pcs[1]])
#	rug(pca$x[,pcs[2]], side = 2)
	title(xlab = txt.x, ylab = txt.y)
	legend("topleft", y = NULL, pca$method, bty = "n", cex = 0.75)
		
	if (is.numeric(tol)) labelExtremes(p, pl, tol)
	}


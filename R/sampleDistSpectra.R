#'
#'
#' Compute the Distance Between Samples in a Spectra Object
#' 
#' Compute the Distance between samples in a Spectra object.  This is a means
#' to quantify the similarity between samples.  A heat map style plot is an
#' option.
#' 
#' 
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param method Character.  A string giving the distance method.  See
#' \code{\link{rowDist}} for options.
#'
#' @param plot Logical.  Shall a level plot be made?
#'
#' @param \dots Arguments to be passed to the plotting function.
#'
#' @return A numeric matrix giving the correlation coefficients.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso The sample distances can be used to cluster the samples. See for
#' example \code{\link{hcaSpectra}}.
#'
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#'
#' @keywords hplot
#'
#' @examples
#' 
#' require("lattice")
#' data(SrE.NMR)
#' M <- sampleDistSpectra(SrE.NMR, main = "Sample Correlations for SrE.NMR")
#' 
#' @export sampleDistSpectra
#'
#' @importFrom grDevices rainbow
# @importFrom lattice levelplot
#'
sampleDistSpectra <- function(spectra, method = "pearson", plot = TRUE, ...) {
	
# Function to compute sample (row)-wise distances
# of a Spectra object
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, January 2015

	if (!requireNamespace("lattice", quietly = TRUE)) {
		stop("You need to install package lattice to use this function")
	}
	
	if (missing(spectra)) stop("No spectral data provided")
	chkSpectra(spectra)

	M <- rowDist(spectra$data, method)
	
	if (plot) { # M is class dist, need true matrix to plot
		myc <- rev(rainbow(20, start = 0.0, end = 0.66))
		p <- lattice::levelplot(as.matrix(M), xlab = "sample", ylab = "sample",
			col.regions = myc,
			at = seq(-1.0, 1.0, by = 0.1), ...)
		print(p)
		}
		
	return(M)
	}

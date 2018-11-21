#'
#' Compute the Distance Between Samples in a Spectra Object
#' 
#' Compute the Distance between samples in a Spectra object.  This is a means
#' to quantify the similarity between samples.  A heat map style plot is an
#' option.
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
#' example \code{\link{hcaSpectra}}. Additional documentation
#' at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#'
#' @keywords hplot
#'
#' @examples
#' 
#' require("lattice")
#' data(SrE.IR)
#' M <- sampleDistSpectra(SrE.IR, method = "cosine",
#'   main = "SrE.IR Spectral Angle Between Samples")
#' 
#' @export sampleDistSpectra
#'
#' @importFrom grDevices rainbow
# @importFrom lattice levelplot
#'
sampleDistSpectra <- function(spectra, method = "pearson", plot = TRUE, ...) {
	
	if (!requireNamespace("lattice", quietly = TRUE)) {
		stop("You need to install package lattice to use this function")
	}
	
	.chkArgs(mode = 11L)
	chkSpectra(spectra)

	M <- rowDist(spectra$data, method)
		
	if (plot) { # M is class dist, need true matrix to plot
		myc <- rev(rainbow(20, start = 0.0, end = 0.66))
		
		if (method == "correlation") {
			p <- lattice::levelplot(as.matrix(M), xlab = "sample", ylab = "sample",
				col.regions = myc,
				at = seq(-1.0, 1.0, by = 0.1), ...)
			print(p)
		}

		if (!method == "correlation") {
			p <- lattice::levelplot(as.matrix(M), xlab = "sample", ylab = "sample",
				col.regions = myc, ...)
			print(p)
		}

		} # end of if (plot)
		
	return(M)
	}

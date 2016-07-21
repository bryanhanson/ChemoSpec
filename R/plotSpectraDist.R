#'
#'
#'
#' Plot the Distance Between Spectra in a Spectra Object
#' 
#' This function plots the distance between a reference spectrum and all other
#' spectra in a \code{\link{Spectra}} object.  Distance can be defined in a number of
#' ways (see Arguments).
#' 
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param method Character.  Any method acceptable to \code{\link{rowDist}}.
#'
#' @param ref Integer.  The spectrum to be used as a reference.
#'
#' @param labels Logical.  Shall the points be labeled?
#'
#' @param \dots Plot parameters to be passed to the plotting routines.
#'
#' @return A data frame containing the data plotted (sample names, sample
#' colors, distances).
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#'
#' @keywords hplot multivariate
#'
#' @examples
#' 
#' data(SrE.NMR)
#' txt1 <- paste("Distance from", SrE.NMR$names[1]) # capture before padding
#' txt2 <- paste("Rank Distance from", SrE.NMR$names[1])
#' SrE.NMR$names <- paste("  ", SrE.NMR$names, sep = "") # pad the names for better appearance
#' temp <- plotSpectraDist(SrE.NMR, xlab = txt2, ylab = txt1, main = txt1,
#'   xlim = c(1,16), ylim = c(0, 0.3), srt = 90)
#' 
#' @export plotSpectraDist
#'
#' @importFrom graphics plot text
#' @importFrom stats dist
#' @importFrom plyr arrange
#'
plotSpectraDist <- function(spectra, method = "pearson", ref = 1, labels = TRUE, ...) {
	
# Compute distances between spectra and display the results
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, March 2016
	
	#print(length(ref))
	#if (length(ref > 1L)) stop("ref should be a single number")
	if (missing(spectra)) stop("No spectral data provided")
	chkSpectra(spectra)
	
	DM <- as.matrix(rowDist(spectra$data, method = method))
	dimnames(DM) <- list(spectra$names, spectra$names)
	d <- DM[,ref]
	d <- d[-ref]
	newcols <- spectra$colors[-ref]	
	newnames <- spectra$names[-ref]
	DF <-  data.frame(name = newnames, col = newcols, dist = d, stringsAsFactors = FALSE)
	DF <- arrange(DF, dist)
	
	if (labels) {
		plot(x = 1:nrow(DF), y = DF$dist, type = "p", col = DF$col, pch = 20, ...)
		text(x = 1:nrow(DF), y = DF$dist, labels = DF$name, cex = 0.5, adj = c(0, 0), ...)
		}
	
	if (!labels) plot(x = 1:nrow(DF), y = DF$dist, type = "p", col = DF$col, pch = 20, ...)
	
	return(DF)
	}

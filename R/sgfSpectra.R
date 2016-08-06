#'
#'
#' Apply Savitzky-Golay filters to a Spectra object
#' 
#' This function is a simple wrapper around the function
#' \code{\link[signal]{sgolayfilt}}.  It allows one to apply Savitzky-Golay
#' filters to a \code{\link{Spectra}} object in a convenient way.
#' 
#' 
#' @param spectra An object of S3 class \code{\link{Spectra}} to be checked.
#'
#' @param m The desired m-th derivative.  \code{m = 0} smooths the data (i.ei a rolling
#' average), \code{m = 1} gives the first derivative etc.
#'
#' @param \dots Other parameters to be passed to
#' \code{\link[signal]{sgolayfilt}}.
#'
#' @return A object of class \code{\link{Spectra}}.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#'
#' @keywords utilities multivariate
#'
#' @examples
#' 
#'   data(SrE.IR)
#'   myt1 <- expression(bolditalic(Serenoa)~bolditalic(repens)~bold(IR~Spectra))
#'   myt2 <- expression(bolditalic(Serenoa)~bolditalic(repens)~bold(IR~Spectra~(Smoothed)))
#' 
#'   par(mfrow = c(2, 1))
#'   plotSpectra(SrE.IR, xlim = c(1900, 2100), yrange = c(0, 0.05), main = myt1)
#'   temp <- sgfSpectra(SrE.IR)
#'   plotSpectra(temp, xlim = c(1900, 2100), yrange = c(0, 0.05), main = myt2)
#'   par(mfrow = c(1, 1))
#' 
#' @export sgfSpectra
#'
# @importFrom signal sgolayfilt
#'
sgfSpectra <- function(spectra, m = 0, ...) {
	
# Function to filter a Spectra object
# Bryan Hanson, DePauw University, Feb 2016

	if (!requireNamespace("signal", quietly = TRUE)) {
		stop("You need to install package signal to use this function")
		}	

	if (missing(spectra)) stop("No spectral data provided")
	chkSpectra(spectra)
	
	
	for (i in 1:length(spectra$names)) {
		spectra$data[i,] <- signal::sgolayfilt(spectra$data[i,], m = m, ...)
		}

	chkSpectra(spectra)
	return(spectra)
	}

#'
#'
#'
#' Hierarchical Cluster-Based Peak Alignment on a Spectra Object
#' 
#' This function is a wrapper to several functions in the \pkg{speaq} package.
#' It implements the CluPA algorithm described in the reference.
#' 
#' 
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#' 
#' @param bT Numeric.  The baseline threshold. Defaults to five percent of the
#' range of the data, in \code{spectra$data}.  Passed to
#' \code{detectSpecPeaks}.
#' 
#' @param \dots Other arguments to be passed to the underlying functions.
#' 
#' @return A modifed \code{\link{Spectra}} object.
#' 
#' @author Bryan A. Hanson, DePauw University. \email{hanson@@depauw.edu}
#' 
#' @references
#' 
#' Vu TN, Valkenborg D, Smets K, Verwaest KA, Dommisse R, Lemiere F, Verschoren
#' A, Goethals B, Laukens K. "An integrated workflow for robust alignment and
#' simplified quantitative analysis of NMR spectrometry data" BMC
#' Bioinformatics vol. 12 pg. 405 (2011).
#' 
#' \url{https://github.com/bryanhanson/ChemoSpec}
#' 
#' @keywords utilities
#' 
#' @examples
#' 
#' # July 2017: This function requires pkg speaq which in turn
#' # requires pkg data.table, which is broken in the CRAN build
#' # chain.  We make a check of the status so we can pass CRAN!
#'
#' if (requireNamespace("speaq", quietly = TRUE)) {
#'
#' data(alignMUD)
#'
#' plotSpectra(alignMUD, which = 1:20, lab.pos = 4.5, offset = 0.1,
#'   yrange = c(0, 1900), amp = 500, xlim = c(1.5, 1.8),
#'   main = "Misaligned NMR Spectra (alignMUD)")
#'
#' aMUD <- clupaSpectra(alignMUD)
#' plotSpectra(aMUD, which = 1:20, lab.pos = 4.5, offset = 0.1,
#'   yrange = c(0, 1900), amp = 500, xlim = c(1.5, 1.8),
#'   main = "Aligned NMR Spectra (alignMUD)")
#' 
#' } # end of namespace check
#'
#' @export clupaSpectra
#'
# @importFrom speaq detectSpecPeaks findRef dohCluster
#'
clupaSpectra <- function(spectra, bT = NULL, ...) {
	# Wrapper function to carry out hierarchical cluster-based peak alignment
	# for NMR spectra (after Vu, Laukens, Valkenborg)
	# Part of the ChemoSpec package
	# Bryan Hanson, DePauw University, January 2015

	if (!requireNamespace("speaq", quietly = TRUE)) {
		stop("You need to install package speaq to use this function")
		}
		
	if (is.null(bT)) bT <- 0.05*diff(range(spectra$data)) + abs(min(spectra$data))
		pL <- speaq::detectSpecPeaks(spectra$data, baselineThresh = bT, ...)
		ref <- speaq::findRef(pL)[[1]]
		spectra$data <- speaq::dohCluster(spectra$data, pL, ref, ...)
		return(spectra)
	}

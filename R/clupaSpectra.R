
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

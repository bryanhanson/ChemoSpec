
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

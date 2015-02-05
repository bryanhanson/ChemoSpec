

baselinespectra <- function(spectra, int = TRUE, retC = FALSE, ...) {
	
	# A simple wrapper to the excellent baseline package
	# Part of ChemoSpec.  Bryan Hanson December 2011
	
	if (int) baselineGUI(spectra$data, ...) # no return value
	if (!int) {
		b <- baseline(spectra$data, ...)
		plot(b)
		
		if (retC) {
			bc <- getCorrected(b)
			spectra$data <- bc
			chkspectra(spectra)
			return(spectra)
			}
			
		if (!retC) return(b)
		}
	}
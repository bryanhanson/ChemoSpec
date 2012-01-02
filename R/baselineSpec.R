

baselineSpec <- function(Spectra, int = TRUE, retC = FALSE, ...) {
	
	# A simple wrapper to the excellent baseline package
	# Part of ChemoSpec.  Bryan Hanson December 2011
	
	if (int) baselineGUI(Spectra$data, ...) # no return value
	if (!int) {
		b <- baseline(Spectra$data, ...)
		plot(b)
		
		if (retC) {
			bc <- getCorrected(b)
			Spectra$data <- bc
			chkSpectra(Spectra)
			return(Spectra)
			}
			
		if (!retC) return(b)
		}
	}
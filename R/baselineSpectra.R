

baselineSpectra <- function(spectra, int = TRUE, retC = FALSE, ...) {
	
	# A simple wrapper to the excellent baseline package
	# Part of ChemoSpec.  Bryan Hanson December 2011
		
	dat <- spectra$data # possible conflict with baseline's use of spectra
	if (int) baselineGUI(dat, ...) # no return value
	if (!int) {
		b <- baseline(dat, ...)
		plot(b)
		
		if (retC) {
			bc <- getCorrected(b)
			spectra$data <- bc
			chkSpectra(spectra)
			return(spectra)
			}
			
		if (!retC) return(b)
		}
	}

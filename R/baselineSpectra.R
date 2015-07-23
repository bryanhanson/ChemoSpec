

baselineSpectra <- function(spectra, int = TRUE, retC = FALSE, ...) {
	
	# A simple wrapper to the excellent baseline package
	# Part of ChemoSpec.  Bryan Hanson December 2011

	if (!requireNamespace("baseline", quietly = TRUE)) {
		stop("You need to install package baseline to use this function")
		}

	dots <- list(...)
	if (any(names(dots) == "method")) {
	    method <- dots$method
		if (method == "rfbaseline") {
			if (!requireNamespace("IDPmisc", quietly = TRUE)) {
				stop("You need to install package IDPmisc to use method rfbaseline")
				}
			}
	    }
		
	dat <- spectra$data # possible conflict with baseline's use of spectra
	if (int) baseline::baselineGUI(dat, ...) # no return value
	if (!int) {
		b <- baseline::baseline(dat, ...)
		baseline::plot(b)
		
		if (retC) {
			bc <- baseline::getCorrected(b) # the way it is supposed to be done...
			# works interactively, but not in vignette ???
			#bc <- b@corrected
			spectra$data <- bc
			chkSpectra(spectra)
			return(spectra)
			}
			
		if (!retC) return(b)
		}
	}

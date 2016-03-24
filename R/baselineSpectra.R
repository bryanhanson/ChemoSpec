

baselineSpectra <- function(spectra, int = TRUE, retC = FALSE, ...) {
	
	# Mostly a simple wrapper to the excellent baseline package
	# Part of ChemoSpec.  Bryan Hanson December 2011

	# Some clunky checking here to send data either to package baseline
	# or handle it locally, with the linear method.
	# "method" is not a formal argument for compatibility with baseline
	# but we do need it if processing locally.
	
	dots <- list(...)
	linear <- FALSE
	if (any(names(dots) == "method")) {
	    method <- dots$method
		if (method == "rfbaseline") {
			if (!requireNamespace("IDPmisc", quietly = TRUE)) {
				stop("You need to install package IDPmisc to use method rfbaseline")
				}
			}
		if (method == "linear") linear <- TRUE
	    }


	if (linear) { # Process locally and return immediately
		np <- length(spectra$freq)
	
	# It would probably be better to use the mean of ~50 points
	# at the beginning and end for the fit
	
	# A fancier version could allow the user to give two ranges
	# where the baseline is just noise, and use those to
	# make the correction.  However, other methods of correction
	# are probably better in that case.
	
		for (i in 1:length(spectra$names)) {
			rMin <- min(spectra$data[i,])
			spectra$data[i,] <- spectra$data[i,] - rMin
			# Do an lm from end to the other
			DF <- data.frame(
				x = c(spectra$freq[1], spectra$freq[np]),
				y = c(spectra$data[i,1], spectra$data[i,np]))
			fit <- lm(y ~ x, DF)
			spectra$data[i,] <- spectra$data[i,]- predict(fit,
				newdata = data.frame(x = spectra$freq))
			}
	
		chkSpectra(spectra)
		return(spectra)
		}

	# Below here, we are using methods in package baseline
		
	if (!requireNamespace("baseline", quietly = TRUE)) {
		stop("You need to install package baseline to use this function")
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

removeSample <-
function(spectra, rem.sam) {
	
# Function to Remove Selected Samples
# Typically used to remove spectra with artifacts
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Aug 2009

	if (missing(spectra)) stop("No spectral data provided")
	if (missing(rem.sam)) stop("Nothing to remove")
	chkSpectra(spectra)
	
	# remove the requested samples by name or number
	# BE CAREFUL: greping can catch more than you think!

	if (is.character(rem.sam)) {
		for (n in 1:length(rem.sam)) rem.sam[n] <- grep(rem.sam[n], spectra$names)
		rem.sam <- as.numeric(rem.sam)
		}
	if (max(rem.sam) >= length(spectra$names)) stop("Samples to remove are out of range")

	spectra$data <- spectra$data[-rem.sam,]
	spectra$names <- spectra$names[-rem.sam]
	spectra$groups <- spectra$groups[-rem.sam]
	spectra$colors <- spectra$colors[-rem.sam]
	
	if (length(spectra$names) == 0) warning("You may have removed all your samples!")

	# other aspects of spectra are untouched

	spectra

	}


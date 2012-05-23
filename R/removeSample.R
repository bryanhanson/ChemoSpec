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
	
	k <- c()
	if (is.character(rem.sam)) {
		for (n in 1:length(rem.sam)) {
			more <- grep(rem.sam[n], spectra$names)
			k <- c(k, more)
			}
		rem.sam <- k
		}

	if (max(rem.sam) > length(spectra$names)) stop("Samples to remove are out of range")

	spectra$data <- spectra$data[-rem.sam,]
	spectra$names <- spectra$names[-rem.sam]
	spectra$groups <- spectra$groups[-rem.sam, drop = TRUE]
	spectra$colors <- spectra$colors[-rem.sam]
	spectra$sym <- spectra$sym[-rem.sam]
	spectra$alt.sym <- spectra$alt.sym[-rem.sam]
	
	if (length(spectra$names) == 0) warning("You have removed all your samples!")

	# other aspects of spectra are untouched
	
	chkSpectra(spectra)
	spectra

	}


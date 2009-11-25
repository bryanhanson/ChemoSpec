normSpectra <-
function(spectra) {
	
# Function to Normalize the data in a Spectra object
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Nov 2009

	if (missing(spectra)) stop("No spectral data provided")
	chkSpectra(spectra)
	
	for (n in 1:length(spectra$names)) {
		S <- sum(spectra$data[n,])
		spectra$data[n,] <- spectra$data[n,]/S
		}
	chkSpectra(spectra)
	spectra
	}

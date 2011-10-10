normSpectra <-
function(spectra, method = "ti") {
	
# Function to Normalize the data in a Spectra object
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Nov 2009

	if (missing(spectra)) stop("No spectral data provided")
	chkSpectra(spectra)

# normalize by a row by the sum of its entries:

	if (method == "ti") {
		for (n in 1:length(spectra$names)) {
			S <- sum(spectra$data[n,])
			spectra$data[n,] <- spectra$data[n,]/S
			}
		}

	chkSpectra(spectra)
	spectra
	}

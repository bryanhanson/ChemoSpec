normSpectra <-
function(spectra, method = "TotInt", RangeExpress = NULL) {
	
# Function to Normalize the data in a Spectra object
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Nov 2009

	if (missing(spectra)) stop("No spectral data provided")
	chkSpectra(spectra)

# normalize by a row by the sum of its entries:

	if (method == "TotInt") {
		for (n in 1:length(spectra$names)) {
			S <- sum(as.numeric(spectra$data[n,]))
			spectra$data[n,] <- spectra$data[n,]/S
			}
		}

# normalize by a range of specified values:

	if (method == "Range") {
		if (is.null(RangeExpress)) stop("No range expression given")
		rfi <- which(RangeExpress)
		for (n in 1:length(spectra$names)) {
			S <- sum(as.numeric(spectra$data[n,rfi]))
			spectra$data[n,] <- spectra$data[n,]/S
			}
		}

	chkSpectra(spectra)
	spectra
	}

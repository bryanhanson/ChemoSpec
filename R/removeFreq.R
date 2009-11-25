removeFreq <-
function(spectra, rem.freq) {

# Function to remove selected frequencies from a Spectra object
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Nov 2009

	if (missing(spectra)) stop("No spectral data provided")
	if (missing(rem.freq)) stop("Nothing to remove")
	if (max(rem.freq) > ceiling(max(spectra$freq))) stop("Frequencies to remove exceed maximum in data")
	if (min(rem.freq) < floor(min(spectra$freq))) stop("Frequencies to remove are below minimum in data")
	chkSpectra(spectra)
	
	rem.freq <- findInterval(rem.freq, sort(spectra$freq), rightmost.closed = TRUE)
	spectra$data <- spectra$data[,-rem.freq]
	spectra$freq <- spectra$freq[-rem.freq]
	chkSpectra(spectra)
		
	spectra
	}


removeFreq <-
function(spectra, rem.freq) {

# Function to remove selected frequencies from a Spectra object
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Nov 2009
# Major revision, July 2010

	if (missing(spectra)) stop("No spectral data provided")
	if (missing(rem.freq)) stop("Nothing to remove")
	chkSpectra(spectra)
	
	# rem.freq must be a character string giving a valid logical statement of freq to be removed
	# generally should be a combination of ?Comparison and ?base::Logic concepts.	
	rfi <- which(rem.freq)
	spectra$data <- spectra$data[,-c(rfi)]
	spectra$freq <- spectra$freq[-c(rfi)]
	chkSpectra(spectra)
		
	spectra
	}


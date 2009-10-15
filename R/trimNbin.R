trimNbin <-
function(spectra, rem.freq) {

# Function to Trim Data Range & Bin Data
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, July 2009

	if (missing(spectra)) stop("No spectral data provided")
	if (missing(rem.freq)) stop("Nothing to remove")
	chkSpectra(spectra)

	# bin function to be added later

	# Trim action amounts to a simple editing function to remove undesired frequencies

	# if min or max are passed in rem.freq, need to convert to appropriate integers
	
	if (is.character(rem.freq)) {
		if (rem.freq[1] == "min") {
			n1 <- floor(min(spectra$freq))
			n2 <- as.integer(rem.freq[2])
			rem.freq <- rem.freq[-c(1,2)]
			rem.freq <- c(n1:n2, rem.freq)
			}
		
		a <- length(rem.freq)
		if (rem.freq[a] == "max") {
			n1 <- ceiling(max(spectra$freq))
			n2 <- as.integer(rem.freq[a-1])
			rem.freq <- rem.freq[-c(a-1, a)]
			rem.freq <- c(rem.freq, n2:n1)
			}

		rem.freq <- as.integer(rem.freq)
	
		}
	
	if (max(rem.freq) > ceiling(max(spectra$freq))) stop("Frequencies to remove are out of range (max)")
	if (min(rem.freq) < floor(min(spectra$freq))) stop("Frequencies to remove are out of range (min)")
	
	rem.freq <- findInterval(rem.freq, spectra$freq)
	spectra$data <- spectra$data[,-rem.freq]
	spectra$freq <- spectra$freq[-rem.freq]
	chkSpectra(spectra)

	# other aspects of spectra are untouched

	spectra

	}


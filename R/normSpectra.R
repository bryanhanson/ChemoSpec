normSpectra <-
function(spectra, method = "PQN", RangeExpress = NULL) {
	
# Function to Normalize the data in a Spectra object
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Nov 2009

	if (missing(spectra)) stop("No spectral data provided")
	chkSpectra(spectra)

# normalize using the probablistic quotient normalization (PQN)

	if (method == "PQN") {
		
		# Do a standard TotInt normalization
		S <- normSpectra(spectra, method = "TotInt")$data
		
		# Compute the median spectrum for reference
		M <- apply(S, 2, median)

		# Divide each normed spectra by the column medians
		for (i in 1:nrow(S)) S[i,] <- S[i,]/M
		
		# Get the row medians (per spectrum median)
		# These are the apparent 'fold' dilution factors
		# for each spectrum
		M2 <- apply(S, 1, median)
		
		# Divide each row by it's median
		for (i in 1:nrow(S)) S[i,] <- S[i,]/M2[i]
		
		spectra$data <- S
		}

# normalize a row by the sum of its entries:

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

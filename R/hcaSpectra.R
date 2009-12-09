hcaSpectra <-
function(spectra, title = "no title provided",
method = "complete", use.sym = FALSE, ...) {

# Function to carry out HCA, basically a wrapper to existing methods
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, June 2008

	if (missing(spectra)) stop("No spectral data provided")
	chkSpectra(spectra)	
	
	if (use.sym) spectra$names <- paste(spectra$alt.sym, spectra$names, sep = " ")
	distance <- dist(as.data.frame(spectra$data, row.names = spectra$names))

	title <- paste(title, ": HCA Analysis", sep = "")
	sub.title <- paste("Clustering method: ", method, sep = "")

	plotHCA(spectra = spectra, distance = distance, title = title, sub.title = sub.title,
		method = method, use.sym = use.sym)
	}


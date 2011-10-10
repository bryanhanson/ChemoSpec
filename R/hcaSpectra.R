hcaSpectra <-
function(spectra, title = "no title provided",
c.method = "complete", d.method = "euclidean", use.sym = FALSE, ...) {

# Function to carry out HCA, basically a wrapper to existing methods
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, June 2008

	if (missing(spectra)) stop("No spectral data provided")
	chkSpectra(spectra)	
	
	if (use.sym) spectra$names <- paste(spectra$alt.sym, spectra$names, sep = " ")
	distance <- rowDist(as.data.frame(spectra$data, row.names = spectra$names), method = d.method)

	title <- paste(title, ": HCA Analysis", sep = "")
	sub.title <- paste("clustering method: ", c.method, "      distance method: ", d.method, sep = "")

	plotHCA(spectra = spectra, distance = distance, title = title, sub.title = sub.title,
		method = c.method, use.sym = use.sym)
	}


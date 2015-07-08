hcaSpectra <-
function(spectra,
c.method = "complete", d.method = "euclidean",
use.sym = FALSE, leg.loc = "topright", ...) {

# Function to carry out HCA, basically a wrapper to existing methods
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, June 2008

	if (missing(spectra)) stop("No spectral data provided")
	chkSpectra(spectra)	
	
	if (use.sym) spectra$names <- paste(spectra$alt.sym, spectra$names, sep = " ")
	distance <- rowDist(as.data.frame(spectra$data, row.names = spectra$names), method = d.method)

	sub.title <- paste("clustering method: ", c.method, "      distance method: ", d.method, sep = "")

	hclst <- hclust(distance, method = c.method)

	d <- plotHCA(spectra = spectra, hclst = hclst, sub.title = sub.title,
		use.sym = use.sym, leg.loc = leg.loc, ...)
	L = list(hclst = hclst, dend = d)
	return(L)
	}


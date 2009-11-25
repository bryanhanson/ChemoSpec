hcaSpectra <-
function(spectra, title = "no title provided", method = "complete", ...) {

# Function to carry out HCA, basically a wrapper to existing methods
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, June 2008

	if (missing(spectra)) stop("No spectral data provided")
	chkSpectra(spectra)	
	
	distance <- dist(as.data.frame(spectra$data, row.names = spectra$names))
	cluster <- as.dendrogram(hclust(distance), method = method)
	cluster <- dendrapply(cluster, colLeaf, spectra)
	title <- paste(title, ": HCA Analysis", sep = "")
	sub.title <- paste("Clustering method: ", method, sep = "")
	plot(cluster, main = title, sub = sub.title, horiz = FALSE, ...)
	
	leg.txt <- levels(spectra$group)
	leg.col <- NULL
	for (z in 1:length(leg.txt)) {
		i <- match(leg.txt[z], spectra$group)
		leg.col[z] <- spectra$colors[i]
		}
	leg.txt <- c("Key", leg.txt)
	leg.col <- c("black", leg.col)
	legend("topright", leg.txt, text.col = leg.col, bty = "n")
	}


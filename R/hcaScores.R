hcaScores <-
function(spectra, pca, title = "no title provided", scores = c(1:5), method = "complete", ...) {

# Function to carry out HCA on PCA scores
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Aug 2009

	
	if (missing(spectra)) stop("No spectral data set provided")
	if (missing(pca)) stop("No pca results provided")
	if (!("princomp" %in% class(pca) || "prcomp" %in% class(pca))) stop("Your pca results look corrupt!")
	chkSpectra(spectra)

### need to add conversion of pca modes here

	distance <- dist(as.data.frame(pca$x[,scores], row.names = spectra$names))
	cluster <- as.dendrogram(hclust(distance), method = method)
	cluster <- dendrapply(cluster, colLeaf, spectra)
	title <- paste(title, ": HCA Analysis of PC Scores", sep = "")

	sub.title <- paste("Clustering method:", method, "  PCA method:", pca$method, sep = " ")
	plot(cluster, main = title, sub = sub.title, horiz = FALSE)
	
	leg.txt <- levels(spectra$groups)
	leg.col <- NULL
	for (z in 1:length(leg.txt)) {
		i <- match(leg.txt[z], spectra$groups)
		leg.col[z] <- spectra$colors[i]
		}
	leg.txt <- c("Key", leg.txt)
	leg.col <- c("black", leg.col)
	legend("topright", leg.txt, text.col = leg.col, bty = "n")
	}


plotHCA <-
function(spectra, distance, title, sub.title, method, use.sym, ...) {

# Function to plot HCA results, basically a wrapper to existing methods
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Dec 2009

	cluster <- as.dendrogram(hclust(distance), method = method)
	if (!use.sym) cluster <- dendrapply(cluster, colLeaf, spectra)
	cluster <- dendrapply(cluster, shrinkLeaf, spectra)

	plot(cluster, main = title, sub = sub.title, horiz = FALSE, ...)
	
	gr <- sumGroups(spectra)
	leg.txt <- gr$group
	leg.col <- gr$color
	leg.sym <- gr$alt.sym
	
	leg.txt <- c("Key", leg.txt)
	leg.col <- c("black", leg.col)
	leg.sym <- c("", leg.sym)
	if (!use.sym) legend("topright", leg.txt, text.col = leg.col, bty = "n")
	if (use.sym) legend("topright", leg.txt, text.col = "black", pch = leg.sym, bty = "n")
	}


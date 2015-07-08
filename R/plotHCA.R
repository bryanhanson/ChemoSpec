plotHCA <-
function(spectra, hclst, sub.title, use.sym, leg.loc, ...) {

# Function to plot HCA results, basically a wrapper to existing methods
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Dec 2009

	cluster <- as.dendrogram(hclst)
	if (!use.sym) cluster <- dendrapply(cluster, colLeaf, spectra)
	cluster <- dendrapply(cluster, shrinkLeaf, spectra)

	plot(cluster, sub = sub.title, horiz = FALSE, ...)
	
	if (leg.loc == "none") return(cluster)
	
	gr <- sumGroups(spectra)
	leg.txt <- gr$group
	leg.col <- gr$color
	leg.sym <- gr$alt.sym
	
	leg.txt <- c("Key", leg.txt)
	leg.col <- c("black", leg.col)
	leg.sym <- c("", leg.sym)
	if (!use.sym) legend(leg.loc, leg.txt, text.col = leg.col, bty = "n")
	if (use.sym) legend(leg.loc, leg.txt, text.col = "black", pch = leg.sym, bty = "n")
	return(cluster)
	}


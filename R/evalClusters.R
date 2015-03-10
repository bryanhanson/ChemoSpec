evalClusters <-
function(spectra, pkg = "NbClust", hclst = NULL,  k = NULL, h = NULL, crit = "Dunn", ...) {

# Function to evaluate the quality of clusters found by hcaScores or hcaSpectra
# Basically a wrapper for NbClust and clusterCrit
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Dec 2014

	if (pkg == "NbClust") {
		res <- NbClust(spectra$data, ...)
		return(res)
		}

	if (pkg == "clusterCrit") {
		if (is.null(hclust)) stop("You must provide an hclust object")
	 	ct <- cutree(hclst, k = k, h = h)
		res <- intCriteria(spectra$data, ct, crit, ...)
		return(res)
		}

	}

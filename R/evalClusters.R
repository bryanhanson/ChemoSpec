evalClusters <-
function(spectra, pkg = "NbClust", hclst = NULL,  k = NULL, h = NULL, crit = "Dunn", ...) {

# Function to evaluate the quality of clusters found by hcaScores or hcaSpectra
# Basically a wrapper for NbClust and clusterCrit
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Dec 2014

	if (pkg == "NbClust") {
		if (!requireNamespace("NbClust", quietly = TRUE)) {
			stop("You need install package NbClust to use this function/option")
			}
		res <- NbClust::NbClust(spectra$data, ...)
		}

	if (pkg == "clusterCrit") {
		if (!requireNamespace("clusterCrit", quietly = TRUE)) {
			stop("You need install package clusterCrit to use this function/option")
			}
		if (is.null(hclust)) stop("You must provide an hclust object")
	 	ct <- cutree(hclst, k = k, h = h)
		res <- clusterCrit::intCriteria(spectra$data, ct, crit, ...)
		return(res)
		}
	}

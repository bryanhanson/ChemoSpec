evalClusters <-
function(spectra, hclst,  k = NULL, h = NULL, crit = "Dunn") {

# Function to evaluate the quality of clusters found by hcaScores or hcaSpectra
# Basically a wrapper into XXXX
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Dec 2014

	ct <- cutree(hclst, k = k, h = h)
	res <- intCriteria(spectra$data, ct, crit)
	return(res)
	}


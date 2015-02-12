
	
aovPCAloadings <-function(spectra, LM, pca, plot = 1, loads = 1, ref = 1, ...) {

#  Function to plot LoadingsLoadings of ANOVA-PCA per Harrington
#  Bryan Hanson and Matt Keinsley
#  DePauw University, June 2011

#  LM is the output from aov_pcaSpectra (a list of matrices)

	chkSpectra(spectra)
	if (plot > length(LM)){
		stop("Error, plot does not exist. Please choose a different plot!")}


##  Creation of titles for each graph depending on the number of factors and which graph was specified

	if (length(LM) == 3) {
		if (plot == 1) title = names(LM)[1]
		if (plot == 2) title = names(LM)[2]
		if (plot == 3) title = names(LM)[3]
		}
		
	if (length(LM) == 5) {
		if (plot == 1) title = names(LM)[1]
		if (plot == 2) title = names(LM)[2]
		if (plot == 3) title = names(LM)[3]
		if (plot == 4) title = names(LM)[4]
		if (plot == 5) title = names(LM)[5]
		}
		
	if (length(LM) == 8) {
		if (plot == 1) title = names(LM)[1]
		if (plot == 2) title = names(LM)[2]
		if (plot == 3) title = names(LM)[3]
		if (plot == 4) title = names(LM)[4]
		if (plot == 5) title = names(LM)[5]
		if (plot == 6) title = names(LM)[6]
		if (plot == 7) title = names(LM)[7]
		if (plot == 8) title = names(LM)[8]
		}

	
	plotLoadings(spectra = spectra, pca = pca, title = title, loads = loads, ref = ref, ...)	
	}

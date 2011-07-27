
	
aovPCAscores <- function(spectra, LM, plot = 1, type = "class", choice = NULL, ...) {

# Function to plot Scores of ANOVA-PCA
# Bryan Hanson and Matt Keinsley
# DePauw University, June 2011

# LM is the list of matrices from aovPCA

	if (plot > length(LM)){
		stop("Error, plot does not exist. Please choose a different plot!")}
	
	chkSpectra(spectra)
	
	types <- c("class", "rob")
	check <- type %in% types
	if (!check){stop("PCA option invalid")}
		
	spectra$data <- LM[[plot]] + LM$Res.Error

	if (is.null(choice)) choice = "noscale"
	if (type == "class") pca <- classPCA(spectra, choice = choice, cent = FALSE)
	if (type == "rob") pca <- robPCA(spectra, choice = choice)

#  Create titles for each graph depending on number of factors and which graph was specified. 

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
	
	plotScores(spectra = spectra, pca, title = title, ...)	
	return(pca)
	}
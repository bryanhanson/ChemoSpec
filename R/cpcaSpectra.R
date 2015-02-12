c_pcaSpectra <-
function(spectra, choice = "noscale", cent = TRUE) {

# Function to carry out classical Principal Components Analysis
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Sept 2009
	
	if (missing(spectra)) stop("No spectral data set passed to PCA")
	
	choices <- c("noscale", "autoscale", "Pareto") # trap for invalid scaling method
	check <- choice %in% choices
	if (!check) stop("The choice of scaling parameter was invalid")
	chkSpectra(spectra)

	# Center & scale the data using the desired method.

	if (identical(choice, "noscale")) {centscaled <- scale(spectra$data, center = cent, scale = FALSE)}
	
	if (identical(choice, "autoscale")) {
		col.sd <- apply(spectra$data, 2, sd)
		centscaled <- scale(spectra$data, center = cent, scale = col.sd)}

	if (identical(choice, "Pareto")) {
		col.sd <- apply(spectra$data, 2, sd)
		centscaled <- scale(spectra$data, center = cent, scale = col.sd^0.5)}
	
	# Now the PCA!
	
	pca <- prcomp(centscaled, retx = TRUE, center = FALSE, scale. = FALSE)
	pca$method <- paste("centered/", choice, "/", "classical", sep = "")
	
	pca
				
	}


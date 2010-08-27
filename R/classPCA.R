classPCA <-
function(spectra, choice = "noscale") {

# Function to carry out non-robust Principal Components Analysis
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Sept 2009
	
	if (missing(spectra)) stop("No spectral data set passed to PCA")
	if (!class(spectra) == "Spectra") stop("Your spectral data set looks corrupt!")
	
	choices <- c("noscale", "autoscale", "Pareto") # trap for invalid scaling method
	check <- choice %in% choices
	if (!check) stop("The choice of scaling parameter was invalid")
	chkSpectra(spectra)

	# Center & scale the data using the desired method.

	if (identical(choice, "noscale")) {centscaled <- scale(spectra$data, center = TRUE, scale = FALSE)}
	
	if (identical(choice, "autoscale")) {
		col.sd <- sd(spectra$data)
		centscaled <- scale(spectra$data, center = TRUE, scale = col.sd)}

	if (identical(choice, "Pareto")) {
		col.sd <- sd(spectra$data)
		centscaled <- scale(spectra$data, center = TRUE, scale = col.sd^0.5)}
	
	# Now the PCA!
	
	pca <- prcomp(centscaled, retx = TRUE, center = FALSE, scale. = FALSE)
	pca$method <- paste("centered/", choice, "/", "classical", sep = "")
	
	pca
				
	}


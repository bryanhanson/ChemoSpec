r_pcaSpectra <-
function(spectra, choice = "noscale") {

	if (!requireNamespace("pcaPP", quietly = TRUE)) {
		stop("You need to install package pcaPP to use this function")
	}
	
	if (missing(spectra)) stop("No spectral data set provided")
	chkSpectra(spectra)
	
	choices <- c("noscale", "mad") # trap for invalid scaling method
	check <- choice %in% choices
	if (!check) stop("The choice of scaling parameter for robust PCA was invalid")

	# Note: PCAgrid produces an object of class princomp, not prcomp
	# so there must be some conversion to match class prcomp
	
	note <- choice
	if (choice == "noscale") choice <- NULL
	pca <- pcaPP::PCAgrid(spectra$data, k = 10, scale = choice, scores = TRUE)
	pca$method <- paste("l1median/", note, "/", "robust", sep = "")
	pca <- r2qPCA(pca) # convert classes
	}


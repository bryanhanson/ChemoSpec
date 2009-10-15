robPCA <-
function(spectra, choice = "noscale") {
	
	if (missing(spectra)) stop("No spectral data set provided")
	chkSpectra(spectra)
	
	choices <- c("noscale", "mad") # trap for invalid scaling method
	check <- choice %in% choices
	if (!check) stop("The choice of scaling parameter for robust PCA was invalid")

	# row scale (compensates for different dilutions/handling of samples)
	t.data <- t(spectra$data)
	sums <- colSums(t.data)
	row.scaled <- t(scale(t.data, center = FALSE, scale = sums))

	# Note: PCAgrid produces an object of class princomp, not prcomp
	# so there must be some conversion to match class prcomp
	
	note <- choice
	if (choice == "noscale") choice <- NULL
	pca <- PCAgrid(row.scaled, k = 10, scale = choice, scores = TRUE)
	pca$method <- paste("l1median/", note, "/", "robust", sep = "")
	pca <- r2qPCA(pca) # convert classes
	}


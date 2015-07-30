
hypTestScores <-
	function(spectra, pca, pcs = 1:3, fac = NULL, ...) {

# Function to carry out hypothesis test on PCA scores
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, August 2010

# This conducts a very simple hypothesis test, no contrasts or projections
# Fancier processing might be possible by using ... to pass along lm options

	if (missing(spectra)) stop("No spectral data set provided")
	if (missing(pca)) stop("No pca results provided")
	if (!("princomp" %in% class(pca) || "prcomp" %in% class(pca)))
		stop("Your pca results look corrupt!")
	if (is.null(fac)) stop("No factors specified")
	chkSpectra(spectra)
	
	scores <- pca$x[,pcs] # response vector
	
	# create formula
	
	form <- with(spectra, as.formula(paste("scores", "~", paste(fac, collapse = "*"))))

	
	# Do the hyp test; R knows if scores is multivariate or not,
	# but the summary format differs between aov and manova
	# 

	if (length(pcs) > 1) out <- manova(formula = form, ...)
	if (length(pcs) == 1) out <- aov(formula = form, ...)
	
	invisible(out)
	
	}

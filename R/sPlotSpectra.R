

sPlotSpectra <- function(spectra, pca, pc = 1, tol = 0.05, ...) {

##  Code to produce s-plots from Spectra objects
##  as in Wiklund.  Part of ChemoSpec
##  Matthew J. Keinsley
##  DePauw University, July 2011

	if (length(pc) != 1) stop("You must choose exactly 1 pc to plot.")
	if (missing(spectra)) stop("No spectral data set provided")
	if (missing(pca)) stop("No PCA results provided")
	if (!("princomp" %in% class(pca) || "prcomp" %in% class(pca))) stop("Your pca results look corrupt!")
	chkSpectra(spectra)

	centspec <- scale(spectra$data, scale = FALSE)

	cv <- sdv <- c()
	
# Loop over each variable

	for (i in 1:ncol(centspec)) {
		tmp <- (pca$x[,pc] %*% centspec[,i])
		cv <- c(cv, tmp)
		dv <- sd(as.vector(centspec[,i])) # sd(matrix) deprecated for 2.14
		sdv <- c(sdv, dv)
	} 

	cv <- cv/(nrow(centspec)-1)
  	crr <- cv/(sdv*pca$sdev[pc])
	ans <- data.frame(freq = spectra$freq, cov = cv, corr = crr)

	plot(cv, crr, xlab = "covariance", ylab = "correlation",
		pch = 20, ...)
	abline(v = 0.0, col = "red")
	abline(h = 0.0, col = "red")
	legend("bottomright", y = NULL, pca$method, bty = "n", cex = 0.75)

	# Next, if requested, we will label the extreme points on both dimensions
	
	if (is.numeric(tol)) labelExtremes(ans[,2:3], spectra$freq, tol)	
 
 	ans
	}





sPlotSpectra <- function(spectra, pca, pc = 1, title = "no title provided", ...) {

##  Code to produce s-plots from Spectra objects
##  as in Wiklund.  Part of ChemoSpec
##  Matthew J. Keinsley
##  DePauw University, July 2011

	if (missing(spectra)) stop("No spectral data set passed to PCA")
	chkSpectra(spectra)

	centspec <- scale(spectra$data, scale = FALSE)

	cv <- sdv <- c()
	
# Loop over each variable

	for (i in 1:ncol(centspec)) {
		tmp <- (pca$x[,pc] %*% centspec[,i])
		cv <- c(cv, tmp)
		dv <- sd (centspec[,i])
		sdv <- c(sdv, dv)
	} 

	cv <- cv/(nrow(centspec)-1)
  
	crr <- cv/(sdv*pca$sdev[pc])

	title = paste(title, ": ", "s-Plot", sep = "")
	s <- qplot(x = cv, y = crr, main = title, ylim = c(-1,1), 
		xlab = "covariance", ylab = "correlation", ...) +
	geom_vline (xintercept = 0.0, color = "red") + 
	geom_hline(yintercept = 0.0, color = "red") 

	print(s)
 
	ans <- data.frame(cov = cv, corr = crr)
 	ans
	}


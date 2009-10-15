plot2Loadings <-
function(spectra, pca, title = "no title provided", loads = c(1,2), tol = 0.05, ...) {
	
# Function to plot loadings against each other
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, June 2008

	if (length(loads) != 2) stop("You must choose exactly 2 loadings to plot.")
	if (missing(spectra)) stop("No spectral data set provided")
	if (missing(pca)) stop("No PCA results provided")
	if (!("princomp" %in% class(pca) || "prcomp" %in% class(pca))) stop("Your pca results look corrupt!")
	chkSpectra(spectra)
	
	# pull the requested loadings

	loadings1 = pca$rotation[,loads[1]]
	loadings2 = pca$rotation[,loads[2]]
	
	eigensum <- sum(pca$sdev*pca$sdev) # prepare axis labels
	variance <- 100*(pca$sdev*pca$sdev/eigensum)
	txt1 <- paste("PC", loads[1], " (", format(variance[loads[1]], digits=2), "%", ") loadings", sep = "")
	txt2 <- paste("PC", loads[2], " (", format(variance[loads[2]], digits=2), "%", ") loadings", sep = "")

	title <- paste(title, ": Covariance of Loadings", sep = "")
	
	xrange <- range(loadings1)*c(1.0, 1.05) # makes room for labels
	yrange <- range(loadings2)*c(1.0, 1.05)

	plot(loadings1, loadings2, main = title, xlab = txt1, ylab = txt2, pch = 20, xlim = xrange, ylim = yrange)
	abline(v = 0.0, col = "lightgray")
	abline(h = 0.0, col = "lightgray")
	legend("bottomleft", y = NULL, pca$method, bty = "n", cex = 0.75)

	# Next, if requested, we will label the extreme points on both dimensions
	
	if (is.numeric(tol)) labelExtremes(pca$rotation[,loads], spectra$freq, tol)	
	}



sampleDistSpectra <- function(spectra, method = "pearson", plot = TRUE, ...) {
	
# Function to compute sample (row)-wise distances
# of a Spectra object
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, January 2015

	if (!requireNamespace("lattice", quietly = TRUE)) {
		stop("You need to install package lattice to use this function")
	}
	
	if (missing(spectra)) stop("No spectral data provided")
	chkSpectra(spectra)

	M <- rowDist(spectra$data, method)
	
	if (plot) { # M is class dist, need true matrix to plot
		myc <- rev(rainbow(20, start = 0.0, end = 0.66))
		p <- lattice::levelplot(as.matrix(M), xlab = "sample", ylab = "sample",
			col.regions = myc,
			at = seq(-1.0, 1.0, by = 0.1))
		}
		
	return(M)
	}


sampleCorSpectra <- function(spectra, plot = TRUE, ...) {
	
# Function to compute sample (row)-wise correlations
# on a spectra object
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, January 2015

	# Helper function
	
	rwCor <- function(M, plot = TRUE) {
		nr <- nrow(M)
		cM <- matrix(NA_real_, nr, nr)
		for (i in 1:nr) {
			for (j in 1:nr) {
				if (j >= i) next # skip diag and upper tri
				cM[i,j] <- cor(M[i,], M[j,])
				}
			}
		diag(cM) <- 1.0
	    ind <- upper.tri(cM) # symmetrize to add upper.tri
	    cM[ind] <- t(cM)[ind] 
		if (plot) {
			myc <- rev(rainbow(20, start = 0.0, end = 0.66))
			p <- levelplot(cM, xlab = "sample", ylab = "sample",
				col.regions = myc,
				at = seq(-1.0, 1.0, by = 0.1))
			print(p)
			}
		cM
		}

	if (missing(spectra)) stop("No spectral data provided")
	chkSpectra(spectra)

	CM <- rwCor(spectra$data, plot, ...)
	return(CM)
	}
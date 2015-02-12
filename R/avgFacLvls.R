

avgFacLvls <- function(matrix, fac) {

# Script to replace rows of a matrix with the averages
# of their group (as identified by fac)
# Part of ChemoSpec/aov_pcaSpectra
# Matthew Keinsley
# DePauw University, May 2011

	M <- matrix

	if (!length(fac) == nrow(M)){
		stop("Length of factor must equal number of rows in matrix")
		}

	lev <- levels(fac)
	# cat("Lev = ", lev, "\n")

	for (i in 1:length(lev)) {
		w <- which(fac  == lev[i])
		 # cat("Loop #", i, "\n")
		 # cat("  Level is  ", lev[i], "\n")
		 # cat("  Length of level", i, "is", length(w), "\n")
		 # cat("w = ", w, "\n")
		m <- M[w,] #submatrix for a given level
		avg <- colMeans(m)
		
# Create a matrix
# in which the averages replace the actual spectra
	
		M[w,] <- rep(avg, each = length(w))
		} 
		return(M)
	}


plotSpectraDist <- function(spectra, method = "pearson", ref = 1, labels = TRUE, ...) {
	
# Compute distances between spectra and display the results
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, March 2016
	
	#print(length(ref))
	#if (length(ref > 1L)) stop("ref should be a single number")
	if (missing(spectra)) stop("No spectral data provided")
	chkSpectra(spectra)
	
	DM <- as.matrix(rowDist(spectra$data, method = method))
	dimnames(DM) <- list(spectra$names, spectra$names)
	d <- DM[,ref]
	d <- d[-ref]
	newcols <- spectra$colors[-ref]	
	newnames <- spectra$names[-ref]
	DF <-  data.frame(name = newnames, col = newcols, dist = d, stringsAsFactors = FALSE)
	DF <- arrange(DF, dist)
	
	if (labels) {
		plot(x = 1:nrow(DF), y = DF$dist, type = "p", col = DF$col, pch = 20, ...)
		text(x = 1:nrow(DF), y = DF$dist, labels = DF$name, cex = 0.5, adj = c(0, 0), ...)
		}
	
	if (!labels) plot(x = 1:nrow(DF), y = DF$dist, type = "p", col = DF$col, pch = 20, ...)
	
	return(DF)
	}

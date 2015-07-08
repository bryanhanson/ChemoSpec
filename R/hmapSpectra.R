hmapSpectra <- function(spectra, ...) {
	
	# Function to display heat map of
	# seriated HCA
	# Bryan Hanson, DePauw Univ, July 2010
	# Part of the ChemoSpec package

	chkSpectra(spectra)

	if (!requireNamespace("seriation", quietly = TRUE)) {
		stop("You need to install package seriation to use this function")
		}
		
	x.lab <- paste(spectra$unit[1], ", reordered", sep = "")
	
	res <- seriation::hmap(spectra$data, labRow = spectra$names,
		xlab = x.lab, ylab = "", margins = c(2, 6),
		key.title = "", ...)
		
	DF1 <- data.frame(freq = spectra$freq, rank = res$colInd)
	DF2 <- data.frame(sample = spectra$names, rank = res$rowInd)
	return(list(Freq = DF1, Sample = DF2))
	}
	
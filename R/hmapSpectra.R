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
		xlab = x.lab, ylab = "", margins = c(6, 6),
		key.title = "", ...)
	return(res)
	}
	
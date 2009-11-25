specSurvey <-
function(spectra, title = "No title provided", ...) {

# Script to survey sets of spectra for regions of interest,
# defined as regions with comparatively high std dev
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Aug 2009
	
	if (missing(spectra)) stop("No spectral data set provided")
	chkSpectra(spectra)
	
	title <- paste(title, ": Std Dev of Merged Data Set", sep = "")
	
	col.sd <- spectra$data[1,] # temp definition of proper length
	for(n in c(1:length(col.sd))) {col.sd[n] <- sd(spectra$data[,n])}
	
	plot(spectra$freq, col.sd, xlab = spectra$unit[1], ylab = "std dev",
		main = title, type = "l", ...)

	}


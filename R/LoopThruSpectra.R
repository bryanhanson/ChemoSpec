


loopThruSpectra <- function(Spectra, ...) {

# Function to loop through a series of spectra
# in a Spectra object for one at a time inspection
# Bryan Hanson, DePauw Univ. Oct. 2011

	cat("Type ESC to stop looping through the spectra\n\n")
	nr <- nrow(Spectra$data)
	for (n in 1:nr) {
		tt <- paste("Spectrum #", n, " of ", nr, sep = "")
		plotSpectra(Spectra, which = n, main = tt, ...)
		devAskNewPage(ask = TRUE)
		}
	devAskNewPage(ask = FALSE)
	}

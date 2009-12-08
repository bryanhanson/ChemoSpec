conColScheme <-
function(spectra, old = levels(as.factor(spectra$colors)), new = NULL){
	
# Function to convert color schemes in a Spectra object
# Part of ChemoSpec package
# Bryan Hanson, DePauw Univ, Dec 2009
	
	chkSpectra(spectra) # verify it's legit
	if (!length(old) == length(new)) stop("Old and new color schemes not of same length")
	new.spec <- spectra
	
	for (n in 1:length(old)) {
		which <- grep(old[n], spectra$colors)
		new.spec$colors[which] <- new[n]
		}

	chkSpectra(new.spec)
	new.spec
	}

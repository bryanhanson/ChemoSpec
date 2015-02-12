

splitSpectraGroups <-
function(spectra, inst = NULL, rep.cols = NULL, ...) {

# Function to split groups in a Spectra object
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, July 2010

# Revised June 2011 to work with only one instruction.
# Also added the ability to replace the color scheme
# with a new one corresponding to the new levels.
# This is different effect than conColScheme.

	if (missing(spectra)) stop("No spectral data set provided")
	chkSpectra(spectra)
	if (is.null(inst)) stop("No splitting instructions provided")

	# inst(ructions) must be a list of strings to 
	# be grepped and placed into a new element
	
	lg <- length(spectra$groups)
	li <- length(inst)
	
	# initialize df to carry new groups
	
	tmp <- data.frame(rep(NA, lg))
	colnames(tmp) <- "dummy"
	
	for (i in 1:li) {
		tmp <- data.frame(tmp, rep(NA, lg))
		}
	colnames(tmp) <- c("dummy", names(inst))

	# now load tmp with the actual values
	
	for (i in 1:length(inst)) {
		l <- length(inst[[i]])

		for (j in 1:l) {
			which <- grep(inst[[i]][j], spectra$groups)
			tmp[which,i+1] <- inst[[i]][j]
			}
		tmp[,i+1] <- as.factor(tmp[,i+1])
		}

	spectra <- c(spectra, tmp)
	d <- grep("dummy", names(spectra))
	spectra <- spectra[-d]
	class(spectra) <- "Spectra"
	
	if (!is.null(rep.cols)) {
		if (li > 1) stop("rep.cols can only be used with an instruction composed of one list element\n")
		if (!length(inst[[1]]) == length(rep.cols)) stop("No. repl. colors doesn't equal levels in inst\n")
		
		# Match rep.cols one for one with inst
		l <- length(inst[[1]])
		tmp <- rep(NA, length(spectra$colors))
		
		for (i in 1:l) {
			which <- grep(inst[[1]][i], spectra[[names(inst)[1]]])
			tmp[which] <- rep.cols[i]
			}
		spectra$colors <- tmp
		}
	
	chkSpectra(spectra)
	spectra
	}

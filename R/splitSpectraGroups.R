splitSpectraGroups <-
function(spectra, inst = NULL, ...) {

# Function to split groups in a Spectra object
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, July 2010

	
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
	tmp <- tmp[,-1]
	
	# now load tmp with the actual values
	
	for (i in 1:length(inst)) {
		l <- length(inst[[i]])
		for (j in 1:l) {
			which <- grep(inst[[i]][j], spectra$groups)
			tmp[which,i] <- inst[[i]][j]
			}
		tmp[,i] <- as.factor(tmp[,i])
		}

	spectra <- c(spectra, tmp)
	class(spectra) <- "Spectra"
	chkSpectra(spectra)
	spectra
	}
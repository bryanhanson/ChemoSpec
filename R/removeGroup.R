removeGroup <-
function(spectra, rem.group) {
	
# Function to Remove Selected Groups
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, May 2012

	if (missing(spectra)) stop("No spectral data provided")
	if (missing(rem.group)) stop("Nothing to remove")
	chkSpectra(spectra)
	
	# remove the requested samples by name or number
	# BE CAREFUL: greping can catch more than you think!
	
	k <- c()
	if (is.character(rem.group)) {
		for (n in 1:length(rem.group)) {
			more <- grep(rem.group[n], spectra$groups)
			k <- c(k, more)
			}
		rem.group <- k
		}

	if (max(rem.group) > length(spectra$groups)) stop("Groups to remove are out of range")

	spectra$data <- spectra$data[-rem.group,]
	spectra$names <- spectra$names[-rem.group]
	spectra$groups <- spectra$groups[-rem.group, drop = TRUE]
	spectra$colors <- spectra$colors[-rem.group]
	spectra$sym <- spectra$sym[-rem.group]
	spectra$alt.sym <- spectra$alt.sym[-rem.group]
	
	if (length(spectra$groups) == 0) warning("You have removed all your samples!")

	# other aspects of spectra are untouched

	chkSpectra(spectra)
	spectra

	}


sumSpectra <-
function(spectra, ...){
	
# Function to summarize objects of S3 class 'Spectra'
# Part of ChemoSpec package
# Bryan Hanson, DePauw Univ, Nov 2009
	
	chkSpectra(spectra) # verify it's legit
	
	# Try to guess a sensible value for tol if none provided via the ...
	# This will be passed to check4Gaps
	
	args <- names(as.list(match.call()[-1]))

	tolSet <- FALSE
	if (!("tol" %in% args)) {
		if ((spectra$unit[1] == "ppm") | ((spectra$unit[1] == "chemical shift"))) tol <- 0.01; tolSet <- TRUE
		if (spectra$unit[1] == "wavenumber") tol <- 0.5; tolSet <- TRUE
		if ((spectra$unit[1] == "nm") | ((spectra$unit[1] == "nanometer"))) tol <- 1.05; tolSet <- TRUE
		if (!tolSet) tol <- 0.01
		}

	h <- check4Gaps(spectra$freq, ...)	
	g <- sumGroups(spectra)
	res <- abs(spectra$freq[2] - spectra$freq[1])
	
	cat("\n", spectra$desc, "\n\n")
	cat("\tThere are ", length(spectra$names), " spectra in this set.\n", sep = "")
	cat("\t", "The y-axis unit is ", spectra$unit[2], ".\n\n", sep = "")
	cat("\tThe frequency scale runs from ", spectra$freq[1], " to ", 
		spectra$freq[length(spectra$freq)], " ", spectra$unit[1], "\n", sep = "")
	cat("\tThere are ", length(spectra$freq), " frequency (x-axis) data points.\n", 
		sep = "")
	cat("\tThe frequency resolution is ", res, " ", spectra$unit[1], "/point.\n\n", sep = "")
	if (length(h) > 1) {
		cat("\tThis data set is not continuous along the frequency axis.\n")
		cat("\tHere are the data chunks:\n\n")
		print(h)
		}
	cat("\n")
	cat("\tThe spectra are divided into", length(levels(spectra$groups)), "groups:", "\n\n")

	# Check for extra data and report if found
	sn <- names(spectra)
	tn <- c("freq", "data", "names", "groups", "colors", "sym", "alt.sym", "unit", "desc")
	extra <- setdiff(sn, tn)
	if (length(extra) > 0) {
		msg <- paste("\n\tAdditional data was found:", extra, "\n", sep = " ")
		cat(msg)
		}
	
	cat("\n*** Note: this data is an S3 object of class 'Spectra'\n")
	}


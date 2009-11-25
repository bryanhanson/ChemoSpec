sumSpectra <-
function(spectra, ...){
	
# Function to summarize objects of S3 class 'Spectra'
# Part of ChemoSpec package
# Bryan Hanson, DePauw Univ, Nov 2009
	
	chkSpectra(spectra) # verify it's legit
	h <- check4Gaps(spectra$freq, ...)	
	
	cat(spectra$desc, "\n\n")
	cat("\tThere are ", length(spectra$names), " spectra in this set.\n", sep = "")
	cat("\t", "The y-axis unit is ", spectra$unit[2], ".\n\n", sep = "")
	cat("\tThe frequency scale runs from ", spectra$freq[1], " to ", 
		spectra$freq[length(spectra$freq)], " ", spectra$unit[1], "\n", sep = "")
	cat("\tThere are ", length(spectra$freq), " frequency (x-axis) data points.\n", 
		sep = "")
	res <- abs(spectra$freq[2] - spectra$freq[1])
	cat("\tThe frequency resolution is ", res, " ", spectra$unit[1], "/point.\n\n", sep = "")
	if (length(h) > 1) {
		cat("\tThis data set is not continuous along the frequency axis.\n")
		cat("\tHere are the data chunks:\n\n")
		print(h)
		}
	cat("\n")
	# make a little table to summarize the groups
		
	col.lvls <- c() # match colors to levels
	lvls = levels(spectra$groups)
	count = length(lvls)
	no.n.group <- c()
	
	for (n in 1:count) {
		chk <- match(lvls[n], spectra$groups) # get 1st instance
		col.lvls[n] <- spectra$colors[chk]
		no.n.group[n] <- length(grep(lvls[n], spectra$groups))
		}

	cat("The spectra are divided into", length(levels(spectra$groups)), "groups:", "\n")
	for (n in 1:count) cat("\t", lvls[n], ":", " ", no.n.group[n], " members,",
		" color = ", col.lvls[n], "\n", sep = "")
		

	cat("\n*** Note: this data is an S3 object of class 'Spectra'\n")
	}


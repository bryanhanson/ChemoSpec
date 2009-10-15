sumSpectra <-
function(spectra){
	
# Function to summarize objects of S3 class 'Spectra'
# Part of ChemoSpec package
# Bryan Hanson, DePauw Univ, Aug 2009
	
	chkSpectra(spectra) # verify it's legit
	
### WARNING: no correction is currently made for non-contiguous freq (e.g. post trimNbin)
	
	cat(spectra$desc, "\n")
	cat("\t", "There are ", length(spectra$names), " spectra in this set.\n", sep = "")
	cat("\t", "The frequency scale runs from ", spectra$freq[1], " to ", 
		spectra$freq[length(spectra$freq)], " ", spectra$unit[1], "\n", sep = "")
	res <- (diff(range(spectra$freq)))/length(spectra$freq)
	cat("\t", "The frequency resolution is ", res, " ", spectra$unit[1], "/point.\n", sep = "")
	cat("\t", "The y-axis unit is ", spectra$unit[2], ".\n", sep = "")
	
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
		

	cat("*** Note: this data is an S3 object of class 'Spectra'\n")
	}


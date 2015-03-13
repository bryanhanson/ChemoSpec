readJDX <- function (file = "", debug = FALSE){

# ChemoSpec, Bryan Hanson, November 2012

# This function works with the JCAMP-DX format
# The data block must be of the type XYDATA=(X++(Y..Y))
# Handles AFFN format for the data block and only with '+', '-' or ' ' as the separator
# Not extensively tested

# We need several things out of the JCAMP-DX file format:
# FIRSTX, LASTX, NPOINTS, XYDATA

	if (!requireNamespace("gsubfn", quietly = TRUE)) {
		stop("You need to install package gsubfn to use this option")
	}

	if (file == "") stop("No file specified")
	jdx <- readLines(file)

	# Check for compound JCAMP files, these will have more than one title
	# The standard requires that title be in the first line; this is a check for parsing non-dx files
	
	cmpd <- grep("^##TITLE=.*$", jdx)
	if (cmpd > 1) stop("Compound data sets not supported")
	if (!cmpd == 1) warning("This may not be a JCAMP-DX file")
	
	# Check for NMR data which is not supported
	
	ntup <- grepl("^##NTUPLES", jdx)
	if (any(ntup)) stop("This looks like NMR data with real & imaginary parts, which is not supported")
	
	if (debug) cat("\nFile = ", file, "\n")
	
	# This next grep finds this string: ##XYDATA= (X++(Y..Y)) which is the start of the y data
	spcstart <-  grep("^##XYDATA=\\s*\\(X\\+\\+\\(Y\\.\\.Y\\)\\)$", jdx) + 1
	if (spcstart == 1) stop("Couldn't find the data block start (see ?readJDX for supported formats)")
	
	# And then the end of the y values
#	spcend <- grep("^##END=[[:blank:]]*$", jdx) - 1
	spcend <- grep("^##END=", jdx) - 1 # some files don't end as above
	if (spcend == 0) stop("Couldn't find the data block end")
	
	# Some checks
	if (!length(spcstart) == 1L & length(spcend) == 1L) stop("Problem with delimiting data block")
	if (!spcstart < spcend) stop("End of data block in the wrong place")

	# Each line of the data block begins with a frequency
	# These must be removed via a loop
	# Need to drop everything up to the first separator
	
	yValues <- jdx[spcstart:spcend] # a series of broken strings
	# The first value is a frequency marker, need to remove it
	# It may have a decimal or not, it may have a + or - or
	# spaces or none of the above ahead of it
	# Using sub gets only the first instance (compared to gsub)
	# Aug 2013: added processing of ',' as well as '.' as the decimal point
	for (n in 1:length(yValues)) {
#		yValues[n] <- sub("\\s*(\\+|-)*[[:digit:]]+\\.*[[:digit:]]*(\\+|-|\\s)", "", yValues[n])
		yValues[n] <- sub("\\s*(\\+|-)*[[:digit:]]+(\\.|,)?[[:digit:]]*\\s*", "", yValues[n])
	}
	
	yValues <- paste(yValues, collapse = " ") # concantenated into one long string
	yValues <- gsub("\\+", " ", yValues)
	# replace '+' separators with space (you can have + with no space around it)
	yValues <- gsub("-", " -", yValues) # replace '-' separators with ' -' -- needed to preserve neg values
	yValues <- sub("\\s*", "", yValues) # remove any leading spaces
	yValues <- gsub(",", ".", yValues) # replace ',' with '.' -- needed for EU style files
	yValues <- strsplit(yValues, split = "\\s+") # broken into a vector at each ' '
	yValues <- as.numeric(unlist(yValues))

	# Now get other the values & check a few things, fix up some values too
	
	firstX <- grep("^##FIRSTX=", jdx)
	if (firstX == 0) stop("Couldn't find FIRSTX")
	firstX <- jdx[firstX]
	firstX <- gsubfn::gsubfn("##FIRSTX=", replacement = "", firstX)
	firstX <- sub(",", ".", firstX) # for EU style files
	firstX <- as.numeric(firstX)

	lastX <- grep("^##LASTX=", jdx)
	if (lastX == 0) stop("Couldn't find LASTX")
	lastX <- jdx[lastX]
	lastX <- gsubfn::gsubfn("##LASTX=", replacement = "", lastX)
	lastX <- sub(",", ".", lastX) # for EU style files
	lastX <- as.numeric(lastX)

	npoints <- grep("^##NPOINTS=", jdx)
	if (npoints == 0) stop("Couldn't find NPOINTS")
	npoints <- jdx[npoints]
	npoints <- gsubfn::gsubfn("##NPOINTS=", replacement = "", npoints)
	npoints <- as.integer(npoints)

	if (debug) cat("\tNPOINTS = ", npoints, "\n")
	if (debug) cat("\tActual no. data points found  = ", length(yValues), "\n")

	if (!npoints == length(yValues)) stop("NPOINTS and length of data block don't match")

	if (debug) cat("\tfirstX = ", firstX, "\n")
	if (debug) cat("\tlastX = ", lastX, "\n")
	
	yFac <- grep("^##YFACTOR=", jdx)
	if (yFac == 0) stop("Couldn't find YFACTOR")
	yFac <- gsubfn::gsubfn("##YFACTOR=", replacement = "", jdx[yFac])
	yFac <- sub(",", ".", yFac) # for EU style files
	yFac <- as.numeric(yFac)
	if (debug) cat("\tyFac = ", yFac, "\n")
	yValues <- yValues*yFac

	actDX <- (lastX-firstX)/(npoints - 1)
	xValues <- seq(firstX, lastX, by = actDX)

	res <- data.frame(x = xValues, y = yValues)
}

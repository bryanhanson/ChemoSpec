readJDX <- function (file = ""){

# ChemoSpec, Bryan Hanson, November 2012

# This function works for the JCAMP-DX format for IR Spectra
# Untested with other formats or options
# Handles AFFN format for the data block and only with '+', '-' or ' ' as the separator
# The data block must be of the type XYDATA=(X++(Y..Y))

# We need several things out of the JCAMP-DX file format:
# FIRSTX, LASTX, NPOINTS, XYDATA

	if (file == "") stop("No file specified")
	jdx <- readLines(file)

	# Check for compound JCAMP files, these will have more than one title
	# The standard requires that title be in the first line; this is a check for parsing non-dx files
	
	cmpd <- grep ("^##TITLE=.*$", jdx)
	if (cmpd > 1) stop("Compound data sets not supported")
	if (!cmpd == 1) warning("This may not be a JCAMP-DX file")
	
#	cat("\nFile = ", file, "\n")
	
	# This next grep finds this string: ##XYDATA=(X++(Y..Y)) which is the start of the y data
	spcstart <-  grep ("^##XYDATA=\\(X\\+\\+\\(Y\\.\\.Y\\)\\)$", jdx) + 1
	if (spcstart == 1) stop("Couldn't find the data block start (see ?readJDX for supported formats)")
	
	# And then the end of the y values
	spcend <- grep ("^##END=[[:blank:]]*$", jdx) - 1
	if (spcend == 0) stop("Couldn't find the data block end")
	
	# Some checks
	if (!length(spcstart) == 1L & length(spcend) == 1L) stop("Problem with delimiting data block")
	if (!spcstart < spcend) stop("End of data block in the wrong place")

	# Each line of the data block begins with a frequency
	# These must be removed via a loop
	# Need to drop everything up to the first separator
	
	yValues <- jdx[spcstart:spcend] # a series of broken strings
	for (n in 1:length(yValues)) {
		yValues[n] <- sub("[[:digit:]]+(\\+|-|\\s)", "", yValues[n])
	}
	
	yValues <- paste(yValues, collapse = " ") # concantenated into one long string
	yValues <- gsub("\\+", " ", yValues) # replace '+' separators with space
	yValues <- gsub("-", " -", yValues) # replace '-' separators with ' -' -- needed to preserve neg values	
	yValues <- strsplit(yValues, split = "\\s") # broken into a vector at each ' '
	yValues <- as.numeric(unlist(yValues))

	# Now get other the values & check a few things, fix up some values too
	
	firstX <- grep("^##FIRSTX=[[:digit:]]+$", jdx)
	if (firstX == 0) stop("Couldn't find FIRSTX")
	firstX <- jdx[firstX]
	firstX <- gsubfn("##FIRSTX=", replacement = "", firstX)
	firstX <- as.numeric(firstX)

	lastX <- grep("^##LASTX=[[:digit:]]+$", jdx)
	if (lastX == 0) stop("Couldn't find LASTX")
	lastX <- jdx[lastX]
	lastX <- gsubfn("##LASTX=", replacement = "", lastX)
	lastX <- as.numeric(lastX)

	npoints <- grep("^##NPOINTS=[[:digit:]]+$", jdx)
	if (npoints == 0) stop("Couldn't find NPOINTS")
	npoints <- jdx[npoints]
	npoints <- gsubfn("##NPOINTS=", replacement = "", npoints)
	npoints <- as.integer(npoints)

#	cat("NPOINTS = ", npoints, "\n")
#	cat("length(yValues) = ", length(yValues), "\n")

	if (!npoints == length(yValues)) stop("NPOINTS and length of data block don't match")

#	cat("firstX = ", firstX, "\n")
#	cat("lastX = ", lastX, "\n")
	
	actDX <- (lastX-firstX)/(npoints - 1)
	xValues <- seq(firstX, lastX, by = actDX)

	res <- data.frame(xValues = xValues, yValues = yValues)
}



readBrukerTxt <- function (file = "", debug = FALSE){

	# ChemoSpec, Bryan Hanson, December 2012

	# This function works with the Bruker txt format
	# This is a pretty simple format, we only need a few things from it:
	# LEFT, RIGHT, SIZE
	
	# Not extensively tested!

	if (file == "") stop("No file specified")
	if (debug) cat("\nFile = ", file, "\n")
	tmp <- readLines(file)

	# Get the y values (intensities)
	
	y <- tmp[-grep("#", tmp)]
	y <- as.numeric(y)

	# Now get other the values & check a few things, fix up some values too
	
	firstX <- grep("LEFT", tmp)
	if (firstX == 0) stop("Couldn't find LEFT")
	firstX <- tmp[firstX]
	firstX <- gsub("^#.+LEFT\\s*=\\s*((\\+|-)*[[:digit:]]+\\.*[[:digit:]]*).+", replacement = "\\1", firstX)
	firstX <- as.numeric(firstX)

	lastX <- grep("RIGHT", tmp)
	if (lastX == 0) stop("Couldn't find RIGHT")
	lastX <- tmp[lastX]
	lastX <- gsub(".+RIGHT\\s*=\\s*((\\+|-)*[[:digit:]]+\\.*[[:digit:]]*).+", replacement = "\\1", lastX)
	lastX <- as.numeric(lastX)
	npoints <- grep("SIZE", tmp)
	if (npoints == 0) stop("Couldn't find SIZE")
	npoints <- tmp[npoints]
	npoints <- gsub("^#\\s*SIZE\\s*=\\s*([[:digit:]]+).+", replacement = "\\1", npoints)
	npoints <- as.integer(npoints)

	if (debug) cat("\tSIZE = ", npoints, "\n")
	if (debug) cat("\tActual no. data points found  = ", length(y), "\n")

	if (!npoints == length(y)) stop("SIZE and length of y data don't match")

	if (debug) cat("\tfirstX = ", firstX, "\n")
	if (debug) cat("\tlastX = ", lastX, "\n")
	
	x <- seq(firstX, lastX, length.out = npoints)

	res <- data.frame(x = x, y = y)
}

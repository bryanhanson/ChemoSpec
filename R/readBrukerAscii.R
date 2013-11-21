

readBrukerAscii <- function (file = "", debug = FALSE){

	# ChemoSpec, Bryan Hanson, November 2013

	# This function reads Bruker txt files written with the Topspin
	# command convbin2asc  This file has a descriptive row (which is skipped),
	# then rows with the following columns:
	# row number, peak intensity, frequency, ppm
		
	# Not extensively tested!

	if (file == "") stop("No file specified")
	if (debug) cat("\treadBrukerAscii is reading ", file, "\n")
	tmp <- read.csv(file, header = FALSE, skip = 1)
	y <- tmp[,2]
	x <- tmp[,4]
	if (debug) cat("\tNo. data points = ", length(x), "\n")

	res <- data.frame(x = x, y = y)
}

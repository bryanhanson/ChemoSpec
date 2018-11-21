#'
#' @describeIn files2SpectraObject Import a matrix of data
#'
matrix2SpectraObject <-
function(gr.crit = NULL, gr.cols = c("auto"),
	freq.unit = "no frequency unit provided",
	int.unit = "no intensity unit provided",
	descrip = "no description provided",
	in.file = NULL, out.file = "mydata",
	chk = TRUE, ...) {
		
# Function to Read & Prep Spectroscopic Data
# from a raw data file into an Spectra object
# This function acts on a matrix-like file
# containing frequencies in the first column
# and samples in additional columns.

# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, April 2016

	if (!requireNamespace("R.utils", quietly = TRUE)) {
		stop("You need to install package R.utils to use this function")
		}

	if (is.null(gr.crit)) stop("No group criteria provided to encode data")
	if (is.null(in.file)) stop("You need to specify an input file")

	out <- tryCatch(
	{

	# Get the data matrix
	specs <- read.table(in.file, header = TRUE, ...)
	
	# Set up the Spectra object
	spectra <- list()
	spectra$freq <- as.numeric(specs[,1])
	spectra$data <- as.matrix(t(specs[,-1]))
	dimnames(spectra$data) <- NULL
	spectra$names <- names(specs[-1])
	class(spectra) <- "Spectra"
	
	# Go get group assignments & colors, to complete assembly of spectra

	spectra <- .groupNcolor(spectra, gr.crit, gr.cols, mode = "1D")
	spectra$unit[1] <- freq.unit
	spectra$unit[2] <- int.unit
	spectra$desc <- descrip
	
	if (chk) chkSpectra(spectra)
	
	datafile <- paste(out.file, ".RData", sep = "")
	R.utils::saveObject(spectra, file = datafile)
	return(spectra)
	},
	
	error = function(cond) {
		errmess <- "There was a problem processing your matrix!\n\nDid you get a message such as 'subscript out of bounds'? You probably need to specify sep and possibly dec values. Please read ?files2Spectra2DObject for details.\n\nIf that doesn't fix things, set chk = FALSE and inspect the resulting object.\n"
		message("\nError message from R: ", cond$message, "\n")
		message(errmess)
		return(NA)
		}
	
	) # end of tryCatch
	
	return(out)
	}


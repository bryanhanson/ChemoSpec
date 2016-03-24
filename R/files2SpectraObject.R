files2SpectraObject <-
function(gr.crit = NULL, gr.cols = c("auto"),
	freq.unit = "no frequency unit provided",
	int.unit = "no intensity unit provided",
	descrip = "no description provided",
	format = "csv",
	out.file = "mydata", debug = FALSE, ...) {
		
# Function to Read & Prep Spectroscopic Data
# from raw data files into an Spectra object
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Aug 2009

# This function acts on all files in the working directory

# CSV files should have freq in column 1, absorbance/intensity in column 2.
# There should be no column labels.

# DX files can be parsed (see readJDX for limitations)

	if (!requireNamespace("R.utils", quietly = TRUE)) {
		stop("You need to install package R.utils to use this function")
		}

	if (is.null(gr.crit)) stop("No group criteria provided to encode data")

	# First set up some common stuff
	
	if ((format == "csv") | (format == "csv2")) pat = "\\.(csv|CSV)$"
	if (format == "dx") pat = "\\.(dx|DX|jdx|JDX)$"
	files <- list.files(pattern = pat)
	if ((format == "csv") | (format == "csv2")) {
		files.noext <- substr(basename(files), 1, nchar(basename(files)) - 4)
		}
	if (format == "dx") files.noext <- substr(basename(files), 1, nchar(basename(files)) - 3)		
	spectra <- list() # OK to initialize a list this way
	spectra$names <- files.noext
			
	
	if (debug) message("\nfiles2SpectraObject is checking the first file")
	if (format == "csv") temp <- read.csv(files[1], header = FALSE, ...)
	if (format == "csv2") temp <- read.csv2(files[1], header = FALSE, ...)
	if (format == "dx") {
		temp <- readJDX(file = files[1], debug = debug)
		}

	spectra$freq <- temp[,1]
	if (class(spectra$freq) == "integer") {
		message("\nConverting integer frequency values to numeric")
		spectra$freq <- as.numeric(spectra$freq)
		}
	
	spectra$data <- matrix(data = NA, nrow = length(files), ncol = length(spectra$freq))
	
	# loop over all files (you have to read the whole file then grab
	# just the part you want, i.e. the 2nd column)

	if (debug) message("\nfiles2SpectraObject will now import your files")
	for (i in 1:length(files)) {
		if (debug) cat("Importing file: ", files[i], "\n")
		if (format == "csv") temp <- read.csv(files[i], header = FALSE, ...)
		if (format == "csv2") temp <- read.csv2(files[i], header = FALSE, ...)
		if (format == "dx") temp <- readJDX(files[i], debug = debug, ...)
		spectra$data[i,] <- temp[,2]
		}

	# Go get group assignments & colors, to complete assembly of spectra

	spectra <- groupNcolor(spectra, gr.crit, gr.cols)
	spectra$unit[1] <- freq.unit
	spectra$unit[2] <- int.unit
	spectra$desc <- descrip
	chkSpectra(spectra)
	
	datafile <- paste(out.file, ".RData", sep = "")

	R.utils::saveObject(spectra, file = datafile)
	
	return(spectra)
	}


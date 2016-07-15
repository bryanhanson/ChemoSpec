files2SpectraObject <-
function(gr.crit = NULL, gr.cols = c("auto"),
	freq.unit = "no frequency unit provided",
	int.unit = "no intensity unit provided",
	descrip = "no description provided",
	fileExt = "\\.(csv|CSV)$",
	out.file = "mydata", debug = FALSE, ...) {
		
# Function to Read & Prep Spectroscopic Data
# from raw data files into an Spectra object
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Aug 2009

# This function acts on all files in the working directory with the specified extension

# Files should have freq in column 1, absorbance/intensity in column 2.
# There may or may not be a header (default is FALSE for read.table)

# DX files can be parsed, but are handled separately (see readJDX for limitations)

	if (!requireNamespace("R.utils", quietly = TRUE)) {
		stop("You need to install package R.utils to use this function")
		}

	message("The default behavior of this function has changed as of July 2016.  See ?files2SpectraObject")
	
	if (is.null(gr.crit)) stop("No group criteria provided to encode data")
	
	DX = FALSE
	if (grepl("(dx|DX|jdx|JDX)", fileExt)) DX <- TRUE

	# First set up some common stuff
	
	files <- list.files(pattern = fileExt)
	files.noext <- tools::file_path_sans_ext(files)

	spectra <- list()
	spectra$names <- files.noext

	if (debug) message("\nfiles2SpectraObject is checking the first file")
	if (!DX) temp <- read.table(files[1], ...)
	if (DX) temp <- readJDX(file = files[1], debug = debug)

	spectra$freq <- temp[,1]
	if (class(spectra$freq) == "integer") {
		if (debug) message("\nConverting integer frequency values to numeric")
		spectra$freq <- as.numeric(spectra$freq)
		}
	
	spectra$data <- matrix(data = NA_real_, nrow = length(files), ncol = length(spectra$freq))
	
	# Loop over all files (you have to read the whole file then grab
	# just the part you want, i.e. the 2nd column)

	if (debug) message("\nfiles2SpectraObject will now import your files")
	
	for (i in 1:length(files)) {
		if (debug) cat("Importing file: ", files[i], "\n")
		if (!DX) temp <- read.table(files[i], ...)
		if (DX) temp <- readJDX(files[i], debug = debug, ...)
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


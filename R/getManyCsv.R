getManyCsv <-
function(gr.crit = NULL, gr.cols = c("auto"),
	freq.unit = "no frequency unit provided",
	int.unit = "no intensity unit provided",
	descrip = "no description provided",
	format = "csv",
	out.file = "mydata") {
		
# Function to Read & Prep Spectroscopic Data
# from raw data files into an RData file/object
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Aug 2009
# This script reads a series of csv files that share a common frequency axis.
# Files should have freq in column 1, absorbance/intensity in column 2.
# There should be no column labels.
# Groups & colors assigned using file names by a separate function.

### As of Nov 2012 this is deprecated in favor of files2SpectraObject

	cat("getManyCsv will be deprecated soon.  Please switch to files2SpectraObject")
	
	if (is.null(gr.crit)) stop("No group criteria provided to encode data")

	files <- list.files(pattern = "\\.(csv|CSV)$")
		
	# get the first file, initialize stuff, and pull out the frequency data

	if (format == "csv") temp <- read.csv(files[1], header = FALSE)
	if (format == "csv2") temp <- read.csv2(files[1], header = FALSE)
	spectra <- list() # OK to initialize a list this way
	spectra$freq <- temp[,1]
	if (class(spectra$freq) == "integer") {
		cat("Converting integer frequency values to numeric\n")
		spectra$freq <- as.integer(spectra$freq)
		}
	spectra$data <- matrix(data = 0.0, nrow = length(files), ncol = length(spectra$freq))
	
	# loop over all files (you have to read the whole file then grab
	# just the part you want, i.e. the 2nd column)

	files.noext <- substr(basename(files), 1, nchar(basename(files)) - 4)

	for (i in 1:length(files)) {
		if (format == "csv") temp <- read.csv(files[i], header = FALSE)
		if (format == "csv2") temp <- read.csv2(files[i], header = FALSE)
		spectra$data[i,] <- temp[,2]
		spectra$names[i] <- files.noext[i] # need to remove the .csv
		}

	# go get group assignments & colors, to complete assembly of spectra

	spectra <- groupNcolor(spectra, gr.crit, gr.cols)
	
	spectra$unit[1] <- freq.unit
	spectra$unit[2] <- int.unit
	spectra$desc <- descrip
	
	datafile <- paste(out.file, ".RData", sep = "")
	saveObject(spectra, file = datafile)
	}


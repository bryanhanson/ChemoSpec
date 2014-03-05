files2SpectraObject <-
function(gr.crit = NULL, gr.cols = c("auto"),
	freq.unit = "no frequency unit provided",
	int.unit = "no intensity unit provided",
	descrip = "no description provided",
	format = "csv", alignTMS = FALSE,
	out.file = "mydata", debug = FALSE, ...) {
		
# Function to Read & Prep Spectroscopic Data
# from raw data files into an Spectra object
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Aug 2009

# This function acts on all files in the working directory

# Two main options: alignTMS = FALSE where freq range is the same
# for all files, and alignTMS = TRUE for when they must be aligned.
# The latter case is quite experimental and is intended for NMR
# spectra only, and it must be stored high ppm -> low ppm
# There is currently NO check for this state.

# CSV files should have freq in column 1, absorbance/intensity in column 2.
# There should be no column labels.

# DX files can be parsed (see readJDX for limitations)
# txt files written by Bruker are parsed by readBrukerTxt
# Files written by Bruker Topspin command convbin2asc can are parsed with readBrukerAscii.

	if (is.null(gr.crit)) stop("No group criteria provided to encode data")

	# First set up some common stuff
	
	if ((format == "csv") | (format == "csv2")) pat = "\\.(csv|CSV)$"
	if (format == "dx") pat = "\\.(dx|DX|jdx|JDX)$"
	if (format == "Btxt") pat = "\\.(txt|TXT)$"
	if (format == "Bascii") pat = "\\.(txt|TXT)$"
	files <- list.files(pattern = pat)
	if ((format == "csv") | (format == "csv2") | (format == "Btxt") | (format == "Bascii")) {
		files.noext <- substr(basename(files), 1, nchar(basename(files)) - 4)
		}
	if (format == "dx") files.noext <- substr(basename(files), 1, nchar(basename(files)) - 3)		
	spectra <- list() # OK to initialize a list this way
	spectra$names <- files.noext
			
	# Now the two master loops, alignTMS = T/F
	
	if (!alignTMS) { # default, assumes files all have same npoints

	# As this is simpler, there is less reporting when debug = TRUE
	
		if (debug) message("\nfiles2SpectraObject is checking the first file")
		if (format == "csv") temp <- read.csv(files[1], header = FALSE)
		if (format == "csv2") temp <- read.csv2(files[1], header = FALSE)
		if (format == "dx") {
			temp <- readJDX(file = files[1], debug = debug)
			}
		if (format == "Btxt") {
			temp <- readBrukerTxt(file = files[1], debug = debug)
			}
		if (format == "Bascii") {
			temp <- readBrukerAscii(file = files[1], debug = debug)
			}
	
		spectra$freq <- temp[,1]
		if (class(spectra$freq) == "integer") {
			message("\nConverting integer frequency values to numeric")
			spectra$freq <- as.integer(spectra$freq)
			}
		
		spectra$data <- matrix(data = NA, nrow = length(files), ncol = length(spectra$freq))
		
		# loop over all files (you have to read the whole file then grab
		# just the part you want, i.e. the 2nd column)

		if (debug) message("\nfiles2SpectraObject will now import your files")
		for (i in 1:length(files)) {
			if (debug) cat("Importing file: ", files[i], "\n")
			if (format == "csv") temp <- read.csv(files[i], header = FALSE)
			if (format == "csv2") temp <- read.csv2(files[i], header = FALSE)
			if (format == "dx") temp <- readJDX(files[i], debug = debug)
			if (format == "Btxt") temp <- readBrukerTxt(files[i], debug = debug)
			if (format == "Bascii") temp <- readBrukerAscii(files[i], debug = debug)
			spectra$data[i,] <- temp[,2]
			}
				
	} # end of alignTMS = FALSE
	
	if (alignTMS) {
		# These files may differ in number of data points
		# or have the same number of points but TMS position varies

		if (!format %in% c("Btxt", "Bascii")) stop("alignTMS only works for format = 'Btxt' or 'Bascii'")
		message(">>> Locating and aligning spectra on TMS/TSP signal.")
		message(">>> This is experimental! -- Check the results carefully.")
		
		# Find the largest data set and use that to set up the matrix
		
		maxNP <- 0L
		best <- 0L

		if (debug) message("\nfiles2SpectraObject is looking for the spectrum with the most data points")
		
		for (i in 1:length(files)) {
			if (format == "Btxt") temp <- readBrukerTxt(files[i], debug = debug)
			if (format == "Bascii") temp <- readBrukerAscii(files[i], debug = debug)
			if (dim(temp)[1] > maxNP) {
				maxNP <- dim(temp)[1]
				best <- i
				}
			}

		if (debug) cat("\n\tMax no. data points =", maxNP, "\n")
		if (debug) message("\nfiles2SpectraObject is inspecting the biggest spectrum")
		if (debug) message("& will ask findTMS to do its job.")

		spectra$data <- matrix(data = NA, nrow = length(files), ncol = maxNP)
		#temp <- readBrukerTxt(files[best])
		if (format == "Btxt") temp <- readBrukerTxt(files[best], debug = debug)
		if (format == "Bascii") temp <- readBrukerAscii(files[best], debug = debug)
		spectra$data[best,] <- temp[,2]
		spectra$freq <- temp[,1]
		if (class(spectra$freq) == "integer") {
			message("Converting integer frequency values to numeric")
			spectra$freq <- as.integer(spectra$freq)
			}
		
		if (!exists("span")) span <- 120
		if (!exists("sn")) sn <- 1000
		TMS <- findTMS(spectra$data[best,], span, sn)
		if (debug) cat("Best TMS =", TMS, "\n\n")

		# Now, bring in the rest of the spectra, aligning on TMS or TSP
		# Find the index for TSP in the one spectrum that has been processed

		if (debug) message("\nfiles2SpectraObject will now import each spectrum & align TMS")
		
		for (i in 1:length(files)) {
			
			cat("\nAligning spectrum ", files[i], "\n")

			if (i == best) next # Already processed this one
			
			#temp <- readBrukerTxt(files[i])
			if (format == "Btxt") temp <- readBrukerTxt(files[i], debug = debug)
			if (format == "Bascii") temp <- readBrukerAscii(files[i], debug = debug)
			currNP <- length(temp[,2])
			tms <- findTMS(temp[,2], span, sn)
			dTMS <- TMS - tms
			if (debug) cat("dTMS =", dTMS, "\n\n")
			
			if (currNP == maxNP) {
				# This is a perfect fit so it could drop right in,
				# but it may not align correctly.
				# May need to shift with loss of points
				# on one side or the other
				
				if (dTMS > 0) { # need to shift spectrum right
					cat("\tTruncating and shifting right...\n")
					temp <- temp[1:(currNP - dTMS),] # truncate on the right
					spectra$data[i,(dTMS):length(temp[,2])] <- temp[,2]
					cat("\tTruncated spectrum on the right by", dTMS, "points\n")
					next
					}
				
				if (dTMS == 0) { # just drop it in
					spectra$data[i,] <- temp[,2]
					next
					}
					
				if (dTMS < 0) { # need to shift spectrum left
					cat("\tTruncating and shifting left...\n")
					# cat("dTMS = ", dTMS, "\n")
					# cat("currNP = ", currNP, "\n")
					# str(temp)
					temp <- temp[abs(dTMS):currNP,] # truncate on the left
					spectra$data[i,1:length(temp[,2])] <- temp[,2]
					cat("\tTruncated spectrum on the left by", abs(dTMS), "points\n")
					next
					}
				}
			
			# What's left is spectra that are shorter and align easily!
						
			spectra$data[i,(dTMS):(currNP + dTMS - 1)] <- temp[,2]
						
		} # end of file processing loop
		
		# Shift the frequency scale so that TMS is on 0.0
			
		shift <- spectra$freq[TMS]
		shift <- ifelse(shift > 0, shift, -shift)
		spectra$freq <- spectra$freq + shift
		
		# And finally, trim the whole thing so that there are no NA's left
		
		locNA <- function(x) which(!is.na(x))
		st <- apply(spectra$data, 1, FUN = function(x) min(locNA(x)))
		st <- max(st)
		end <- apply(spectra$data, 1, FUN = function(x) max(rev(locNA(x))))
		end <- min(end)
		spectra$data <- spectra$data[,st:end]
		spectra$freq <- spectra$freq[st:end]

	} # end of alignTMS = TRUE
	
	# Go get group assignments & colors, to complete assembly of spectra

	spectra <- groupNcolor(spectra, gr.crit, gr.cols)
	
	spectra$unit[1] <- freq.unit
	spectra$unit[2] <- int.unit
	spectra$desc <- descrip
	chkSpectra(spectra)
	
	datafile <- paste(out.file, ".RData", sep = "")
	saveObject(spectra, file = datafile)
	spectra
	}


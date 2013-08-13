chkSpectra <-
function(spectra, confirm = FALSE) {

# Function to Check the Integrity of Spectra Objects
# Related to the concept of validObject for S4 classes
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Aug 2009
# Many kinds of errors may be missed due to coercion:
# e.g. spectra$colors[22] <- 1.2 creates "1.2" as the entry
# Only complete replacement of $colors with all integers throws an error.
	
	if (missing(spectra)) stop("No object of supposed class Spectra provided")
	w <- FALSE
	if (!class(spectra) == "Spectra") { warning("The object provided was not of class Spectra"); w <- TRUE }
	if (!class(spectra$freq) == "numeric") { warning("The frequency data appear to be corrupt"); w <- TRUE }
	if (!class(spectra$data) == "matrix") { warning("The spectral intensities appear to be corrupt"); w <- TRUE }
	if (!class(spectra$names) == "character") { warning("The sample names appear to be corrupt"); w <- TRUE }
	if (!class(spectra$color) == "character") { warning("The assigned colors appear to be corrupt"); w <- TRUE }
	if (!class(spectra$unit) == "character") { warning("The units appear to be corrupt"); w <- TRUE }
	if (!class(spectra$desc) == "character") { warning("The description appears to be corrupt"); w <- TRUE }
	if (!class(spectra$groups) == "factor") { warning("The assigned groups appear to be corrupt"); w <- TRUE }
	
	f <- length(spectra$freq)
	d2 <- dim(spectra$data)[2]
	n <- length(spectra$names)
	g <- length(spectra$groups)
	co <- length(spectra$colors)
	d1 <- dim(spectra$data)[1]
	sy <- length(spectra$sym)
	ay <- length(spectra$alt.sym)
	
	if (!identical(f, d2)) { warning("The dimensions don't make sense (freq, data)"); w <- TRUE }
	if (!identical(n, g)) { warning("The dimensions don't make sense (names, group)"); w <- TRUE }
	if (!identical(g, co)) { warning("The dimensions don't make sense (group, colors)"); w <- TRUE }
	if (!identical(co, d1)) { warning("The dimensions don't make sense (colors, data)"); w <- TRUE }
	if (!identical(co, sy)) { warning("The dimensions don't make sense (colors, symbols)"); w <- TRUE }
	if (!identical(sy, ay)) { warning("The dimensions don't make sense (symbols, alt symbols)"); w <- TRUE }
	
	# Add a check for extra list elements (useful with HyperChemoBridge conversions)

	if ((length(spectra) > 9 ) && (confirm)) {
		reqd <- c("freq", "data", "names", "groups", "colors",
			"sym", "alt.sym", "unit", "desc")
		spc <- spectra[!names(spectra) %in% reqd]
		message(">>> Extra data was found in the spectra object:")
		str(spc)
		}
	
	if ((!w) && (confirm)) message(">>> You must be awesome: These spectra look just dandy!")
	if (w) {
		message("*** There seem to be one or more problems with these spectra!")
		stop("Sorry, we can't continue this way: It's not me, it's you!")
		}
	
	}


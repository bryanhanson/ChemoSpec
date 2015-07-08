

covSpectraJS <- function(spectra, freq = spectra$freq[1],
	R = NULL, V = NULL, browser = NULL, minify = TRUE, ...) {

# Function to carry out Nicholson's STOCSY analysis
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University

# This is the interactive JS version May 2015

	if (missing(spectra)) stop("No spectral data provided")
	chkSpectra(spectra)

	# Check to see if spectra$freq is increasing - if not, findInterval will fail
	# Silently reverse things
	if (is.unsorted(spectra$freq)) {
		spectra$freq <- rev(spectra$freq)
		spectra$data <- spectra$data[,ncol(spectra$data):1]
		}

	row <- findInterval(freq, spectra$freq)

	if (is.null(R)) { # user did not provide pre-computed correlation matrix
		X <- spectra$data
		if (ncol(X) > 10000) message("Calculating cor() may take a few minutes")
		R <- cor(X)
		}

	if (is.null(V)) { # user did not provide pre-computed covariance matrix
		X <- spectra$data
		if (ncol(X) > 10000) message("Calculating cov() may take a few minutes")
		V <- cov(X)
		}

	# Color scale for each level
	# blue/low -> red/high, anchored at zero (index 5, a shade of green)
	# max and min will come from the data (i.e., red will be at max of V)
	cscale <- c(rev(rainbow(4, start = 0.45, end = 0.66)), rev(rainbow(5, start = 0.0, end = 0.25)))
	# view with:
	# pie(rep(1, 9), col = cscale)
	cscale <- substr(cscale, 1, 7) # needed as JS doesn't accept alpha channel

	refscale <- seq(-1, 1, length.out = 9)

	# Now average every contiguous pair of values in R[row,] so that there is one
	# less value, and use the mean value of the pair to assign colors
	# e.g. the mean of points n & n+1 determines the color used to plot that segment

	cd <- diff(R[row,])
	cr <- 0.5 * cd + R[row,][-length(R[row,])] # this will have one value less than the data
	myc <- cscale[findInterval(cr, refscale)] # color based upon cor, not cov

	myc <- substr(myc, 1, 7) # needed as JS doesn't accept alpha channel

	#cat("There are", length(myc), "colors.  The first 10 are:\n")
	#print(myc[1:10])
	#cat("There are", length(spectra$freq), "data points\n")

	ylab <- paste("Covariance at Frequency = ", sprintf("%5.5f", spectra$freq[row]), sep = "")

	# Ready to plot

	if (requireNamespace("jsonlite", quietly = TRUE)) {

		# Break the pieces of the Spectra object out into
		# separate JSON entities
		# These will be global variables in the JavaScript

		Freq <- jsonlite::toJSON(spectra$freq)
		Y <- jsonlite::toJSON(V[row,])
		xUnit <- jsonlite::toJSON(spectra$unit[1])
		yLabel <- jsonlite::toJSON(ylab)
		Desc <- jsonlite::toJSON(spectra$desc)
		Dx <- jsonlite::toJSON(range(spectra$freq))
		Dy <- jsonlite::toJSON(range(V[row,]))
		myc <- jsonlite::toJSON(myc)
		keyScale <- jsonlite::toJSON(cscale)
		driver <- jsonlite::toJSON(freq)

		# Prepare for writing
		# Groups commented out as it is not currently used

		data1 <- paste("var Freq = ", Freq, sep = "")
		data2 <- paste("var Y = ", Y, sep = "")
		data3 <- paste("var xUnit = ", xUnit, sep = "")
		data4 <- paste("var yLabel = ", yLabel, sep = "")
		data5 <- paste("var Desc = ", Desc, sep = "")
		data6 <- paste("var Dx = ", Dx, sep = "")
		data7 <- paste("var Dy = ", Dy, sep = "")
		data8 <- paste("var myc = ", myc, sep = "")
		data9 <- paste("var keyScale = ", keyScale, sep = "")
		data10 <- paste("var driver = ", driver, sep = "")

		# Get the JavaScript modules & related files

#		td <- getwd()
		td <- tempdir()
		fd <- system.file("extdata", package = "ChemoSpec")
		cSfiles <- c("cS.css", "cS_globals.js", "cS_controls.js",
		"cS_brushNguides.js", "cS_main.js", "covSpectraJS.html", "cS_spectra.js")
		chk2 <- file.copy(from=file.path(fd, cSfiles), to=file.path(td, cSfiles),
			 overwrite = TRUE)
		if (!all(chk2)) stop("Copying to temporary directory failed")

		js1 <- readLines(con = file.path(td,"cS_globals.js"))
		js2 <- readLines(con = file.path(td,"cS_brushNguides.js"))
		js3 <- readLines(con = file.path(td,"cS_controls.js"))
		js4 <- readLines(con = file.path(td,"cS_spectra.js"))
		js5 <- readLines(con = file.path(td,"cS_main.js"))

		# The following are used to wrap the entire JS code in a
		# scoping function so that performance is improved,

		scopeFunHeader <- "(function() {"
		scopeFunTail <- "})();"

		# Combine, then optionally minify for faster performance, write

		text = c(scopeFunHeader, data1, data2, data3, data4,
			data5, data6, data7, data8, data9, data10,
			js1, js2, js3, js4, js5, scopeFunTail)

		if (minify) {
			if (requireNamespace("js", quietly = TRUE)) {
				text <- js::uglify_optimize(text, unused = FALSE)
				}
			if (!requireNamespace("js", quietly = TRUE)) {
				stop("You need install package js to minify the JavaScript code")
				}
			}


		writeLines(text, sep  = "\n", con = file.path(td,"cS.js"))

		# Open the file in a browser

		pg <-  file.path(td,"covSpectraJS.html")
		if (!is.null(browser)) {
		    browseURL(pg, browser = browser)
			} else {
			# open in RStudio if viewer is not null
		    # similar to htmltools::html_print
				viewer <- getOption("viewer")
			  	if (is.null(browser) && !is.null(viewer)) {
		      		viewer(pg)
			  		} else {
			    		browseURL(pg)
			  			}
			}

		message("The covSpectraJS web page is in the following\ntemp directory which is deleted when you quit R: ")
		message(td)
		return(invisible())
	}

	if (!requireNamespace("jsonlite", quietly = TRUE)) {
		stop("You need install package jsonlite to use this function")
		}

	L <- list(cov = V, cor = R)
	invisible(L)

	}

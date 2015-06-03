
plotSpectraJS <- function(spectra, browser = NULL, minify = TRUE) {

	# Bryan A. Hanson, DePauw University, February 2015
	# This is the R front end controlling everything

	if (missing(spectra)) stop("No spectral data provided")
	chkSpectra(spectra)

	# Check to see if spectra$freq is increasing - if not, the scales will be inverted
	# Silently reverse things
	if (is.unsorted(spectra$freq)) {
		spectra$freq <- rev(spectra$freq)
		spectra$data <- spectra$data[,ncol(spectra$data):1]
		}

	if (requireNamespace("jsonlite", quietly = TRUE)) {

		# Break the pieces of the Spectra object out into
		# separate JSON entities
		# These will be global variables in the JavaScript
	
		Freq <- jsonlite::toJSON(spectra$freq)
		D0 <- jsonlite::toJSON(spectra$data)
		D1 <- jsonlite::toJSON(spectra$data)
		Names <- jsonlite::toJSON(paste(" ", spectra$names, sep = "  "))
		Groups <- jsonlite::toJSON(spectra$groups)
		Colors <- jsonlite::toJSON(spectra$colors)
		xUnit <- jsonlite::toJSON(spectra$unit[1])
		Desc <- jsonlite::toJSON(spectra$desc)
		Dx <- jsonlite::toJSON(range(spectra$freq))
		Dy <- jsonlite::toJSON(range(spectra$data))
	
		# Note: D1 is a copy of the data which will be modified for plotting
		# D0, the original data, and will not be changed
	
		# This vector will keep track of which spectra are to be plotted
	 	sampleBOOL <- c(1L, rep(0, length(spectra$names)-1))
		sampleBOOL <- jsonlite::toJSON(sampleBOOL)
	
		# Prepare for writing
		# Groups commented out as it is not currently used
	
		data1 <- paste("var Freq = ", Freq, sep = "")
		data2 <- paste("var D0 = ", D0, sep = "")
		data3 <- paste("var D1 = ", D1, sep = "")
		data4 <- paste("var Names = ", Names, sep = "")
		data5 <- paste("var Groups = ", Groups, sep = "")
		data6 <- paste("var Colors = ", Colors, sep = "")
		data7 <- paste("var xUnit = ", xUnit, sep = "")
		data8 <- paste("var Desc = ", Desc, sep = "")
		data9 <- paste("var Dx = ", Dx, sep = "")
		data10 <- paste("var Dy = ", Dy, sep = "")
		data11 <- paste("var sampleBOOL = ", sampleBOOL, sep = "")
	
		# Get the JavaScript modules & related files
	
		td <- tempdir()
		fd <- system.file("extdata", package = "ChemoSpec")
		pSfiles <- c("pS.css", "pS_globals.js", "pS_controls.js",
		"pS_brushNguides.js", "pS_main.js", "plotSpectraJS.html", "pS_spectra.js")
		chk2 <- file.copy(from=file.path(fd, pSfiles), to=file.path(td, pSfiles),
			overwrite = TRUE)
		if (!all(chk2)) stop("Copying to temporary directory failed")
	
		js1 <- readLines(con = file.path(td,"pS_globals.js"))
		js2 <- readLines(con = file.path(td,"pS_brushNguides.js"))
		js3 <- readLines(con = file.path(td,"pS_controls.js"))
		js4 <- readLines(con = file.path(td,"pS_spectra.js"))
		js5 <- readLines(con = file.path(td,"pS_main.js"))
	
		# The following are used to wrap the entire JS code in a
		# scoping function so that performance is improved.
	
		scopeFunHeader <- "(function() {"
		scopeFunTail <- "})();"
	
		# Combine, then optionally minify for faster performance, write
	
		text = c(scopeFunHeader, data1, data2, data3, data4,
			data5, data6, data7, data8, data9, data10, data11,
			js1, js2, js3, js4, js5, scopeFunTail)
	
		if (minify) {
			if (requireNamespace("js", quietly = TRUE)) {
				text <- js::uglify_optimize(text, unused = FALSE)
				}
			if (!requireNamespace("js", quietly = TRUE)) {
				stop("You need install package js to minify the JavaScript code")
				}
			}
	
	
		writeLines(text, sep  = "\n", con = file.path(td,"pS.js"))
	
		# Open the file in a browser
	
		pg <-  file.path(td,"plotSpectraJS.html")
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
	
		message("The plotSpectraJS web page is in the following\ntemp directory which is deleted when you quit R: ")
		message(td)
		return(invisible())
	}
	
	if (!requireNamespace("jsonlite", quietly = TRUE)) {
		stop("You need install package jsonlite to use this function")
		}
	}

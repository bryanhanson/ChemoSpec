#'
#' Plot a Spectra Object Interactively
#' 
#' This function uses the d3.js JavaScript library by Mike Bostock to plot a \code{\link{Spectra}}
#' object interactively.  This is most useful for data exploration.  For high
#' quality plots, consider \code{\link{plotSpectra}}.
#' 
#' The spectral data are incorporated into the web page. Keep in mind that very large
#' data sets, like NMR spectra with 32K points, will bog down the browser.
#' In these cases, you may need to limit the number of samples in passed to this function.
#' See \code{\link{removeSample}} or use argument \code{which}.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}} to be checked.
#'
#' @param which Integer.  If not \code{NULL}, specifies by number which spectra to plot.
#' If greater control is needed, use \code{\link{removeSample}} which is more flexible
#' before calling this function.
#'
#' @param browser Character.  Something that will make sense to your OS.  Only
#' necessary if you want to override your system specified browser as
#' understood by \code{R}.  See below for further details.
#'
#' @param minify Logical.  Shall the JavaScript be minified?  This improves
#' performance.  However, it requires package \code{js} which in turn requires
#' package \code{V8}.  The latter is not available on all platforms.  Details
#' may be available at \url{https://github.com/jeroenooms/v8}
#'
#' @return None; side effect is an interactive web page.  The temporary
#' directory containing the files that drive the web page is written to the
#' console in case you wish to use those files.  This directory is deleted when
#' you quit R.  If you wish to read the file, don't minify the code, it will be
#' unreadable.
#'
##' @section Browser Choice: The browser is called by
##' \code{\link[utils]{browseURL}}, which
##' in turn uses \code{options("browser")}.  Exactly how this is handled
##' is OS dependent.
##'
##' @section RStudio Viewer: If browser is \code{NULL}, you are using RStudio, and a
#' viewer is specified, this will be called.  You can stop this by with
#' \code{options(viewer = NULL)}.
##'
##' @section Browser Choice (Mac): On a Mac, the default browser is called
##' by \code{/bin/sh/open}
##' which in turn looks at which browser you have set in the system settings.  You can
##' override your default with
##' \code{browser = "/usr/bin/open -a 'Google Chrome'"} for example.
##'
##' @section Browser Choice & Performance:  You can check the performance of
##' your browser at peacekeeper.futuremark.com  The most relevant score
##' is the rendering category.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso \code{\link{plotSpectra}} for non-interactive plotting.  Details
#' about \code{d3.js} are at \url{www.d3js.org}. Additional documentation at
#' \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#'
#' @keywords plot
#'
#' @examples
#' 
#' if (interactive()) {
#'   require("jsonlite")
#'   require("js")
#'   data(metMUD2)
#'   plotSpectraJS(metMUD2)
#' }
#' 
#' @export plotSpectraJS
#'
#' @importFrom utils browseURL
#'
plotSpectraJS <- function(spectra, which = NULL, browser = NULL, minify = TRUE) {

	# This is the R front end controlling everything

	.chkArgs(mode = 11L)
	chkSpectra(spectra)

	if (!is.null(which)) {
		which <- as.integer(which)
		which2 <- setdiff(1:length(spectra$names), which)
		spectra <- removeSample(spectra, rem.sam = which2)
		}
	
	if (!requireNamespace("jsonlite", quietly = TRUE)) {
		stop("You need install package jsonlite to use this function")
		}

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
	
		#scopeFunHeader <- "(function() {"
		#scopeFunTail <- "})();"
	
		# Combine, then optionally minify for faster performance, write
	
		text = c(data1, data2, data3, data4,
			data5, data6, data7, data8, data9, data10, data11,
			js1, js2, js3, js4, js5)
			
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
	
	}

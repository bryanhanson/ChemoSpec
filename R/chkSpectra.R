#'
#'
#' Verify the Integrity of a Spectra Object
#' 
#' Utility function to verify that the structure of a \code{\link{Spectra}}
#' object (an S3 object) is internally consistent. This function can be used
#' after manual editting of a \code{\link{Spectra}} object.  However,
#' in most cases rather than
#' directly manipulating a \code{\link{Spectra}} object, one should manipulate
#' it via \code{\link{removeFreq}}, \code{\link{removeGroup}}
#' or \code{\link{removeSample}}.
#' 
#' This function is similar in spirit to \code{\link{validObject}} in the S4
#' world.  When used at the console, and the object is OK, no message is
#' written unless \code{confirm = TRUE}.  At the console, if there is a
#' problem, messages are issued regardless of the value of \code{confirm}.
#' When used in a function, this function is silent (assuming \code{confirm =
#' FALSE}) unless there is a problem.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra}} to be checked.
#' 
#' @param confirm Logical indicating whether or not to write the results to the
#' console, as would be desirable for interactive use.
#' 
#' @return None; messages will be printed at the console if there is a problem.
#' 
#' @author Bryan A. Hanson, DePauw University.
#' 
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#' 
#' @keywords classes utilities
#' 
#' @examples
#' 
#' data(metMUD1)
#' chkSpectra(metMUD1, confirm = TRUE) # OK
#'
#' # What's next is the wrong way to manipulate a Spectra object.
#' # One should removeSample instead.
#' # We won't run during checking as an error is raised
#'
#' \dontrun{
#'
#' remove <- c(20:40) 
#' metMUD1$freq <- metMUD1$freq[-remove]
#' chkSpectra(metMUD1, confirm = TRUE) # not OK, you didn't listen to me!
#' }
#' 
#' @export chkSpectra
#' 
#' @importFrom utils str 
#' 
chkSpectra <- function(spectra, confirm = FALSE) {

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
	
	# Check for NA's in the data (saves some grief and questions later)
	
	for (i in 1:nrow(spectra$data)) {
		prob <- which(is.na(spectra$data[i,]))
		if (length(prob) > 1L) {
			msg <- paste("NA found in data for", spectra$names[i], ", please inspect/repair")
			message(msg)
			w <- TRUE
		}	
	}
	# Check for extra list elements (useful with HyperChemoBridge conversions)

	if ((length(spectra) > 9 ) && (confirm)) {
		reqd <- c("freq", "data", "names", "groups", "colors",
			"sym", "alt.sym", "unit", "desc")
		spc <- spectra[!names(spectra) %in% reqd]
		message(">>> Extra data was found in the spectra object:")
		str(spc)
		}
	
	# Wrap up
	
	if ((!w) && (confirm)) message(">>> You must be awesome: These spectra look just dandy!")
	if (w) {
		message("*** There seem to be one or more problems with these spectra!")
		stop("Sorry, we can't continue this way: It's not me, it's you!")
		}
	
	}


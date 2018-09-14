#'
#' Display the Spectra in a Spectra Object One at a Time
#' 
#' Plots each spectrum in a \code{\link{Spectra}} object one at a time, and
#' waits for a return in the console before plotting the next spectrum.  Use
#' \code{ESC} to get out of the loop.
#' 
#' 
#' @param Spectra An object of S3 class \code{\link{Spectra}}.
#' 
#' @param \dots Parameters to be passed downstream.
#' 
#' @return None. Side effect is a plot.
#' 
#' @author Bryan A. Hanson, DePauw University.
#' 
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#' 
#' @keywords hplot
#' 
#' @export loopThruSpectra
#' 
#' @importFrom grDevices devAskNewPage 
#' 
#' @examples
#' 
#' \dontrun{
#'
#' data(metMUD1)
#' loopThruSpectra(metMUD1)
#' }
#' 
loopThruSpectra <- function(Spectra, ...) {

# Function to loop through a series of spectra
# in a Spectra object for one at a time inspection
# Bryan Hanson, DePauw Univ. Oct. 2011

	cat("Press ESC to stop looping through the spectra\n\n")
	ns <- length(Spectra$names)
	for (i in 1:ns) {
		tt <- paste(Spectra$names[i], "(#", i, " of ", ns, ")", sep = "")
		plotSpectra(Spectra, which = i, main = tt, ...)
		devAskNewPage(ask = TRUE)
		}
	devAskNewPage(ask = FALSE)
	}

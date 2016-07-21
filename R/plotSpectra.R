#'
#'
#'
#' Plot Spectra Object
#' 
#' Plots the spectra stored in a \code{\link{Spectra}} object.  One may choose
#' which spectra to plot, and the x range to plot.  Spectra may be plotted
#' offset or stacked.  The vertical scale is controlled by a combination of
#' several parameters.
#' 
#' 
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param which An integer vector specifying which spectra to plot, and the
#' order.
#'
#' @param yrange A vector giving the limits of the y axis desired, for instance
#' \code{c(0, 15)}.  This parameter depends upon the range of values in the
#' stored spectra and defaults to the height of the largest peak in the data
#' set.  Interacts with the next two arguments, as well as the number of
#' spectra to be plotted as given in \code{which}.  Trial and error is used to
#' adjust all these arguments to produce the desired plot.
#'
#' @param offset A number specifying the vertical offset between spectra if
#' more than one is plotted.  Set to 0.0 for a stacked plot.
#'
#' @param amplify A number specifying an amplification factor to be applied to
#' all spectra.  Useful for magnifying spectra so small features show up
#' (though large peaks will then be clipped, unless you zoom on the x axis).
#'
#' @param lab.pos A number giving the location for the identifying label.
#' Generally, pick an area that is clear in all spectra plotted.  If no label
#' is desired, give \code{lab.pos} outside the plotted x range.
#'
#' @param showGrid Logical.  Places light gray vertical lines at each tick mark
#' if \code{TRUE}.
#'
#' @param \dots Additional parameters to be passed to plotting functions.
#'
#' @return None.  Side effect is a plot.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso \code{\link{plotSpectraJS}} for the interactive version.
#'
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#'
#' @keywords hplot
#'
#' @examples
#' 
#' data(metMUD1)
#' plotSpectra(metMUD1, main = "metMUD1 NMR Data",
#' 	 which = c(10, 11), yrange = c(0,1.5),
#' 	 offset = 0.06, amplify = 10, lab.pos = 0.5)
#' 
#' 
#' @export plotSpectra
#'
#' @importFrom graphics grid lines text points plot
#'
plotSpectra <- function(spectra, which = c(1),
	yrange = range(spectra$data),
	offset = 0.0, amplify = 1.0,
	lab.pos = mean(spectra$freq),
	showGrid = TRUE, ...) {
	
# Function to plot multiple spectra @ specified expansions & decorate
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, June 2009

	if (missing(spectra)) stop("No spectral data provided")
	chkSpectra(spectra)
	
	# set up and plot the first spectrum
	
	spectrum <- spectra$data[which[1],]*amplify

	plot(spectra$freq, spectrum, type = "n",
		xlab = spectra$unit[1], ylab = spectra$unit[2],
		ylim = yrange,
		frame.plot = FALSE, ...)
	if (showGrid) grid(ny = NA, lty = 1) # grid will be underneath all spectra
	lines(spectra$freq, spectrum, col = spectra$colors[which[1]], ...)
	lab.x <- lab.pos
	spec.index <- findInterval(lab.x, sort(spectra$freq))
	lab.y <- spectrum[spec.index]
	text(lab.x, lab.y, labels = spectra$names[which[1]], pos = 3, cex = 0.75)
	
	which <- which[-1] # first spectrum already plotted so remove it from the list
	count <- 0 # get the other spectra and plot them as well
	for(n in which) {
		count <- count + 1
		spectrum <- (spectra$data[n,]+(offset*count))*amplify
		points(spectra$freq, spectrum, type = "l", col = spectra$colors[n], ...)
		lab.y <- spectrum[spec.index]
		text(lab.x, lab.y, labels = spectra$names[n], pos = 3, cex = 0.75)
		}
	}


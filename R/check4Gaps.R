#'
#' Check for Missing Values (Gaps) in a Spectra Object
#' 
#' This function may be used with a \code{\link{Spectra}} object to see if
#' there are any gaps or discontinuities in the frequency axis.  Gaps may arise
#' when unwanted frequencies are removed (e.g, water peaks in 1H NMR, or
#' uninteresting regions in any kind of spectroscopy).  As written, it can be
#' used to check for gaps in any appropriate numeric vector.  A plot of the
#' gaps is optional.
#' 
#' The basic procedure is to compare x[n + 1] - x[n] for successive values of
#' n.  When this value jumps, there is a gap which is flagged. \code{beg.indx}
#' and \code{end.indx} will always be contiguous as indices must be; it is the
#' \code{x} values that jump or have the gap.  The indices are provided as they
#' are more convenient in some programming contexts.  If not assigned, the
#' result appears at the console.
#' 
#' @param x A numeric vector to be checked for gaps.
#' 
#' @param y An optional vector of y-values which correspond to the \code{x}
#' values.  Only needed if \code{plot = TRUE}.
#' 
#' @param tol A number indicating the tolerance for checking to see if the step
#' between successive \code{x} values are the same.  Depending upon how the
#' \code{x} values are stored and rounded, you may need to change the value of
#' \code{tol} to avoid flagging trivial "gaps".
#' 
#' @param plot Logical indicating if a plot of the gaps should be made.  If
#' \code{TRUE}, \code{y} must be provided.  The plot is labeled consistent with
#' calling this function on a \code{\link{Spectra}} object.
#' 
#' @param silent Logical indicating a "no gap" condition (return value is
#' \code{FALSE}) should not be reported to the console.  Important because
#' \code{check4Gaps} is called iteratively by other functions.
#' 
#' @param \dots Other parameters to be passed to the plot routines if
#' \code{plot = TRUE}, e.g. \code{xlim}.
#' 
#' @return A data frame giving the data chunks found, with one chunk per line.
#' Also a plot if requested.  In the event there are no gaps found,
#' \code{FALSE} is returned. \item{beg.freq }{The first frequency value in a
#' given data chunk.} \item{end.freq }{The last frequency value in a given data
#' chunk.} \item{size }{The length (in frequency units) of the data chunk.}
#' \item{beg.indx }{The index of the first frequency value in the data chunk.}
#' \item{eng.indx }{The index of the last frequency value in the data chunk.}
#' 
#' @author Bryan A. Hanson, DePauw University.
#' 
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#' 
#' @keywords utilities
#' 
#' @examples
#' 
#' x <- seq(from = 5, to = 12, by = 0.1)
#' remove <- c(8:11, 40:45); x <- x[-remove]
#' gaps <- check4Gaps(x)
#'
#' data(SrE.IR)
#' newIR <- removeFreq(SrE.IR, rem.freq = SrE.IR$freq > 1800 & SrE.IR$freq < 2500)
#' check4Gaps(newIR$freq, newIR$data[1,], plot = TRUE)
#' 
#' @export check4Gaps
#' 
#' @importFrom graphics lines rect 
#' 
check4Gaps <- function(x, y = NULL, tol = 0.01, plot = FALSE, silent = FALSE, ...) {
	
# Function to inspect spectral data for gaps & record them
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, November 2009
# Major revision, December 2016 (much leaner code)

	len.x <- length(x)
	xdiff <- abs(diff(x))
	p <- min(xdiff) # nominal freq/pt
	d1 <- x[1] # beg of data chunk by value
	d1i <- 1L # beg of data chunk by index
	d2 <- c() # end of data chunk by value
	d2i <- c() # end of data chunk by index

	# Check for gaps and build up values and indices
	for (i in 1:length(xdiff)) {
		if (!isTRUE(all.equal(xdiff[i], p, tolerance = tol))) { # detects discontinuity
			d1 <- c(d1, x[i+1])
			d1i <- c(d1i, i)
			d2 <- c(d2, x[i])
			d2i <- c(d2i, i+1)
			}	
		}
	# Add the last entry
	d2 <- c(d2, x[len.x])
	d2i <- c(d2i, len.x)

	DF <- data.frame(beg.freq = d1, end.freq = d2, size = NA, beg.indx = d1i, end.indx = d2i)
	DF$size <- DF$end.freq - DF$beg.freq
	
	if ((nrow(DF) == 1) && (!silent)) cat("No gaps were found by check4Gaps\nNo plot will be made\n")
	
	if ((nrow(DF) > 1) && (plot)) {
		if (missing(y)) stop("No y values provided; cannot plot!")
		plot(x, y, type = "l", col = "black", main = "Gaps in Frequency Data",
			ylab = "", xlab = "marked regions are skipped in data set", ...)
		ybottom <- min(y) - 0.1 * diff(range(y))
		ytop <- max(y) + 0.1* diff(range(y)) # only needs to exceed plotting area to fill it
		for (n in 1:(nrow(DF)-1)){
			lines(x = c(DF[n,2], DF[n+1,1]), y = c(y[DF[n,5]], y[DF[n+1,4]]), lty = 2, col = "white")
			rect(xleft = DF[n,2], ybottom, xright = DF[n+1,1], ytop, density = 15, col = "pink")
			}
		}

	if (nrow(DF) == 1)  DF <- FALSE

	DF
	}

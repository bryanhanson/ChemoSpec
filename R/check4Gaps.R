#'
#'
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
#' remove <- 40:45; x <- x[-remove]
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
	
	len.x <- length(x)
	p <- x[2] - x[1] # nominal freq/pt
	d1 <- c() # beg of data chunk by value
	d2 <- c() # end of data chunk by value
	d1i <- c() # beg of data chunk by index
	d2i <- c() # end of data chunk by index
	dend <- data.frame(d2 = NA, d2i = NA) # end of data chunks
	dbeg <- data.frame(d1 = NA, d1i = NA) # beginning of data chunks
		
	for (n in 1:(len.x - 1)) { # locate ends of data chunks
		t <- x[n + 1] - x[n] # WOULDN'T THIS BE BETTER WITH diff?
		if (!isTRUE(all.equal(t, p, tolerance = tol))) { # detects discontinuity
			dend <- rbind(dend, data.frame(d2 = x[n], d2i = n))
			dbeg <- rbind(dbeg, data.frame(d1 = x[n + 1], d1i = n + 1))
			}
		}

	chk <- dim(dend)[1]
	
	if (chk > 2) { # if 2 or more gaps were found, assemble a table of the data chunks
		dend <- dend[-1,] # clean out NAs
		dbeg <- dbeg[-1,]
		tmp <- data.frame(beg.freq = NA, end.freq = NA, size = NA, beg.indx = NA, end.indx = NA)
		first <- data.frame(beg.freq = x[1], end.freq = dend[1,1], size = NA,
			beg.indx = 1, end.indx = dend[1,2])
		last <- data.frame(beg.freq = dbeg[nrow(dbeg),1], end.freq = x[length(x)], size = NA,
			beg.indx = dbeg[nrow(dbeg),2], end.indx = length(x))
	
		for (n in 1:(nrow(dbeg)-1)) {	
			tmp <- rbind(tmp, data.frame(beg.freq = dbeg[n,1], end.freq = dend[n+1, 1],
				size = NA, beg.indx = dbeg[n,2], end.indx = dend[n+1, 2]))
			}

		tmp <- tmp[-1,] # clean out NAs
		df <- rbind(first, tmp, last)
		df[,3] <- abs(df[,2] - df[,1])
		
		}

	if (chk == 2) { # if 1 gap was found, assemble a table of the data chunks
		dend <- dend[-1,] # clean out NAs
		dbeg <- dbeg[-1,]
		first <- data.frame(beg.freq = x[1], end.freq = dend[1,1], size = NA,
			beg.indx = 1, end.indx = dend[1,2])
		last <- data.frame(beg.freq = dbeg[nrow(dbeg),1], end.freq = x[length(x)], size = NA,
			beg.indx = dbeg[nrow(dbeg),2], end.indx = length(x))
	
		df <- rbind(first, last)
		df[,3] <- abs(df[,2] - df[,1])
		
		}
		
	
	if ((chk == 1) && (!silent)) cat("No gaps were found by check4Gaps\nNo plot will be made\n")
	if (chk == 1)  df <- FALSE
	
	if ((chk > 1) && (plot)) {
		if (missing(y)) stop("No y values provided; cannot plot!")
		plot(x, y, type = "l", col = "black", main = "Gaps in Frequency Data",
			ylab = "", xlab = "marked regions are skipped in data set", ...)
		ybottom <- min(y) - 0.1 * diff(range(y))
		ytop <- max(y) + 0.1* diff(range(y)) # only needs to exceed plotting area to fill it
		for (n in 1:(nrow(df)-1)){
			lines(x = c(df[n,2], df[n+1,1]), y = c(y[df[n,5]], y[df[n+1,4]]), lty = 2, col = "white")
			rect(xleft = df[n,2]+p, ybottom, xright = df[n+1,1]-p, ytop, density = 15, col = "pink")
			}
		}

	df
	}

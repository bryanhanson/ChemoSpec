#'
#'
#'
#' Label Extreme Values in a 2D Data Set
#' 
#' A utility function which plots the sample names next to the sample points.
#' The number of samples labeled can be specified by passing it from the
#' calling function. An internal function, not generally called by the user.
#' 
#' 
#' @param data A matrix containing the x values of the points/samples in the
#' first column, and the y values in the second.
#' 
#' @param names A character vector of sample names.  Length must match the
#' number of rows in \code{x}.
#' 
#' @param tol A number describing the fraction of points to be labeled.
#' \code{tol = 1.0} labels all the points; \code{tol = 0.05} labels
#' \emph{approximately} the most extreme 5 percent.  Note that this is simply
#' based upon quantiles, assumes that both x and y are each normally
#' distributed, and treats x and y separately.  Thus, this is not a formal
#' treatment of outliers, just a means of labeling points.  Note too that while
#' this function could deal with groups separately, the way it is called by
#' \code{\link{plotScoresDecoration}} lumps all groups together.
#' 
#' @return None.  Annotates the plot with labels.
#' 
#' @author Bryan A. Hanson, DePauw University.
#' 
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#' 
#' @keywords utilities
#' 
#' @export labelExtremes
#' 
#' @importFrom stats quantile
#' @importFrom graphics text 
#' 
labelExtremes <- function(data, names, tol) {

# Function to label extreme data points
# Part of the ChemoSpec package
# Bryan Hanson, DePauw Univ, Aug 2009
	
	px <- data[,1]
	py <- data[,2]
	pl <- names
	if (is.numeric(pl)) pl <- sprintf("%.2f", pl)
		
	q.x <- quantile(px, probs = c(1.0-tol, tol), na.rm = TRUE)
	sel.x <- (px <= q.x[2]) | (px >= q.x[1])
	keep.x <- subset(px, sel.x)
	keep.x <- match(keep.x, px) # need to keep this & corresponding y
		
	q.y <- quantile(py, probs = c(1.0-tol, tol), na.rm = TRUE)
	sel.y <- (py <= q.y[2]) | (py >= q.y[1])
	keep.y <- subset(py, sel.y)
	keep.y <- match(keep.y, py) # need to keep this & corresponding x
		
	keep <- unique(c(keep.x, keep.y))

	x <- px[keep]
	y <- py[keep]
	l <- pl[keep]

	for(n in c(1:length(x))) {
		text(x[n], y[n], l[n], pos = 4, offset = 0.2, cex = 0.5)
		}
			
	}


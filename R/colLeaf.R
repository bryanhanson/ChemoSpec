#' Color the Leaves of a Dendrogram Based on a Spectra Object
#' 
#' This function colors the leaves of a dendrogram object.  The code was taken
#' from the help files.  An internal function, not generally called by the
#' user.
#' 
#' 
#' @param n A node in a dendrogram object.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#' 
#' @return Returns a node with the label color properties set.
#' 
#' @author Bryan A. Hanson, DePauw University.
#' 
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#' 
#' @keywords utilities cluster color
#' 
#' @export
#' 
#' @importFrom stats is.leaf 
#' @noRd
#' 
.colLeaf <- function(n, spectra) { # this is called iteratively by dendrapply

# A little trick to color leaves properly, derived from the archives
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, June 2008

	if(is.leaf(n)) {
		a <- attributes(n)
		i <- match(a$label, spectra$names)
		
		attr(n, "nodePar") <- c(a$nodePar, list(lab.col = spectra$colors[i],
			pch = NA))
		}
		n
	}


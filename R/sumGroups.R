#'
#'
#' Summarize the Group Parameters of a Spectra Object
#' 
#' This function summarizes the group membership and descriptive parameters of
#' a \code{\link{Spectra}} object.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra}} whose group
#' membership information is desired.
#'
#' @return A data frame as follows.  Note that if there are groups with no
#' members (due to previous use of \code{\link{removeSample}}), these are
#' dropped.  \item{group}{The name of the group.} \item{no.}{The number in the
#' group.} \item{color}{The color assigned to the group.} \item{symbol}{The
#' symbol assigned to the group.} \item{alt.symbol}{The alternative symbol, a
#' lower-case letter, assigned to the group.}
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso For a discussion of general issues of color, see
#' \code{\link{colorSymbol}}.  To summarize the entire object, \code{\link{sumSpectra}}.
#'
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#'
#' @keywords utilities
#'
#' @examples
#' 
#' data(metMUD1)
#' sumGroups(metMUD1)
#' 
#' @export sumGroups
#'
sumGroups <- function(spectra){
	
# Function to summarize groups in a Spectra object
# Part of ChemoSpec package
# Bryan Hanson, DePauw Univ, Dec 2009
	
	chkSpectra(spectra) # verify it's legit

	gr.l <- levels(spectra$group)
	count <- length(gr.l)
	g.sum <- data.frame(group = NA, no. = NA, color = NA,
			symbol = NA, alt.sym = NA)
	
	for (n in 1:count) {
		gi <- match(gr.l[n], spectra$groups) # find index 1st instance
		gr <- gr.l[n] # value of group
		no. <- length(which(gr == spectra$groups))
		col <- spectra$colors[gi] # value of color
		sym <- spectra$sym[gi] # value of symbol
		asym <- spectra$alt.sym[gi] # value of alt symbol
		g.sum <- rbind(g.sum, data.frame(group = gr, no. = no., color = col,
			symbol = sym, alt.sym = asym))
		}
	g.sum <- g.sum[-1,]
	g.sum <- subset(g.sum, no. > 0)
	rownames(g.sum) <- c(1:nrow(g.sum))
	g.sum
	}

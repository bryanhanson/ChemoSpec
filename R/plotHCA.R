#'
#'
#'
#' Plot Dendrogram for Spectra Object
#' 
#' This function plots the results of an HCA analysis of a
#' \code{\link{Spectra}} object.  Not generally called by the user --
#' \code{\link{hcaSpectra}} and \code{\link{hcaScores}} use it (see those pages
#' for examples).
#' 
#' 
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param hclst A \code{\link{hclust}} object.
#'
#' @param sub.title A character string for the subtitle.
#'
#' @param use.sym Logical; if true, the color scheme will be black and
#' lower-case letters will be used to indicate group membership.
#'
#' @param leg.loc Character; if \code{"none"} no legend will be drawn.
#' Otherwise, any string acceptable to \code{\link{legend}}.
#'
#' @param \dots Additional parameters to be passed to the plotting routines.
#'
#' @return An object of class \code{\link{dendrogram}}. Side effect is a plot.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#'
#' @keywords hplot multivariate cluster
#'
#' @export plotHCA
#'
#' @importFrom stats as.dendrogram dendrapply
#' @importFrom graphics plot legend
#'
plotHCA <- function(spectra, hclst, sub.title, use.sym, leg.loc, ...) {

# Function to plot HCA results, basically a wrapper to existing methods
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Dec 2009

	cluster <- as.dendrogram(hclst)
	if (!use.sym) cluster <- dendrapply(cluster, colLeaf, spectra)
	cluster <- dendrapply(cluster, shrinkLeaf, spectra)

	plot(cluster, sub = sub.title, horiz = FALSE, ...)
	
	if (leg.loc == "none") return(cluster)
	
	gr <- sumGroups(spectra)
	leg.txt <- gr$group
	leg.col <- gr$color
	leg.sym <- gr$alt.sym
	
	leg.txt <- c("Key", leg.txt)
	leg.col <- c("black", leg.col)
	leg.sym <- c("", leg.sym)
	if (!use.sym) legend(leg.loc, leg.txt, text.col = leg.col, bty = "n")
	if (use.sym) legend(leg.loc, leg.txt, text.col = "black", pch = leg.sym, bty = "n")
	return(cluster)
	}


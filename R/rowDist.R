#'
#' Compute Distance Between Rows of a Matrix
#' 
#' This function is a wrapper to compute the distance between rows of a matrix
#' using a number of methods.  Some of these are available in package
#' \code{\link{stats}} and some in \code{\link[amap]{Dist}} from package \pkg{amap}.
#' This function determines which method is requested and then the
#' distance calculation is done by the appropriate method. The exception is the
#' cosine distance which is calculated locally.
#' 
#' Methods \code{c("euclidean"}, \code{"maximum"}, \code{"manhattan"}, \code{"canberra"},\code{"binary"},
#' \code{"minkowski")} are sent to function \code{\link{dist}} in package
#' \code{\link{stats}} while methods \code{c("pearson"}, \code{"correlation"},
#' \code{"spearman"}, \code{"kendall")} are handled by \code{Dist} in package \code{amap}.
#' See the respective help pages for details. \code{"cosine"} is handled
#' locally.
#' 
#' @param x A matrix whose rows will be used for the distance calculation.
#'
#' @param method A character; one of \code{c("pearson"}, \code{"correlation"},
#' \code{"spearman"}, \code{"kendall"}, \code{"euclidean"}, \code{"maximum"}, \code{"manhattan"},
#' \code{"canberra"},\code{"binary"}, \code{"minkowski"}, \code{"cosine")}.
#'
#' @return An object of class \code{dist}.
#'
#' @author Bryan A. Hanson, DePauw University.
#' Suggested by and original code written by Roberto Canteri.
#'
#' @keywords utilities
#'
#' @export rowDist
#'
#' @importFrom stats dist as.dist
# @importFrom amap Dist
#'
rowDist <- function(x, method) {

# Function to access a variety of distance methods which turn out to be in different packages,
# except for cosine which is handled here.
# Some code suggested by Roberto Canteri, and used with extremely minor modifications
# Part of ChemoSpec, October 2011,  Bryan Hanson, DePauw University

	method <- match.arg(method, c("pearson", "correlation", "spearman", "kendall",
		"euclidean", "maximum", "manhattan", "canberra","binary", "minkowski",
		"cosine"))

	if (method %in% c("pearson", "correlation", "spearman", "kendall")) {
		if (!requireNamespace("amap", quietly = TRUE)) {
			stop("You need to install package amap to use this function/option")
			}
		distance <- amap::Dist(x, method = method)
		}
		
	if (method %in% c("euclidean", "maximum", "manhattan", "canberra","binary", "minkowski")) {
		distance <- dist(x, method = method)
		}
		
	if ( method == "cosine") { # code by Claudia Beleites/unmixR w/small modifications
		x <- as.matrix(x)
		x <- tcrossprod(x)
		l <- rowSums(x^2)
		l <- sqrt(outer(l, l))
		distance <- as.dist(x/l)
		}

	return(distance)
	}

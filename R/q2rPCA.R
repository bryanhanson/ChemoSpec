#'
#'
#' Conversion Between PCA Classes
#' 
#' Utility to convert objects of S3 class \code{prcomp} (Q-mode PCA) to
#' objects of S3 class \code{princomp} (R-mode PCA) or \emph{vice-versa}.  An
#' internal function, not generally called by the user.
#' 
#' In the conversion, the necessary list elements are added; the old elements
#' are not deleted (and user added list elements are not affected).  To
#' indicate this, the class attribute is updated to include class
#' \code{conPCA}.  The new object can then be used by functions expecting
#' either class prcomp or princomp.  For details of the structure of
#' \code{\link{prcomp}} or \code{\link{princomp}}, see their respective help
#' pages.
#' 
#' @aliases .q2rPCA .r2qPCA
#'
#' @param x An object of either class \code{prcomp} or class
#' \code{princomp}.  It will be converted to a form that can be used by
#' functions expecting either class.
#'
#' @return A list of class \code{conPCA}.  Note that the order of the
#' elements will vary depending upon the direction of conversion.
#' \item{loadings}{ The loadings from \code{princomp}, or a copy of the
#' rotations from \code{prcomp}.} \item{scores}{ The scores from
#' \code{princomp}, or a copy of the x values from \code{prcomp}.}
#' \item{call}{ The call.  Objects of class \code{prcomp} do not store the
#' original call, so a place holder is used. Otherwise the unchanged call from
#' \code{princomp}.} \item{n.obs}{ The number of observations from
#' \code{princomp}, or computed from the 1st dimension of x in
#' \code{prcomp}.} \item{class}{ \code{conPCA} is pre-pended to the
#' existing class.} \item{sdev}{ Unchanged from original.} \item{center}{
#' Unchanged from original.} \item{scale}{ Unchanged from original.}
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso \code{\link{prcomp}}, \code{\link{princomp}}
#'
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#'
#' @keywords classes utilities
#'
#' @export
#' @noRd
#'
.q2rPCA <- function(x) {
	
# Converts objects of class prcomp (Q-mode PCA) to class princomp (R-mode PCA)
# Bryan Hanson, DePauw Univ, Sept 2009
# original is modified by adding list elements (these could be removed to save space)

	if (!"prcomp" %in% class(x)) stop("The PCA object was not of class prcomp")

	# sdev, center and scale for both classes are the same; no change necessary
	
	x$loadings <- x$rotation
	x$scores <- x$x
	x$call <- "No call available (data converted from class prcomp)"
	x$n.obs <- dim(x$x)[1]	
	class(x) <- c("conPCA", class(x))
	x
	}


#'
#' Conversion Between PCA Classes
#'
#' Utility to convert objects of S3 class \code{prcomp} (Q-mode PCA) to
#' objects of S3 class \code{princomp} (R-mode PCA) or \emph{vice-versa}.  An
#' internal function, not generally called by the user.
#'
#' In the conversion, the required list elements are renamed; the old elements
#' are removed to save space. Other list elements, including user added elements
#' are not changed.  The result can fully pass as the new class.  However, the order
#' of elements is not the same, so access the elements via name not position.
#' The class attribute is updated.
#' For details of the structure of
#' \code{\link{prcomp}} or \code{\link{princomp}}, see their respective help
#' pages.
#'
#' @aliases .q2rPCA .r2qPCA
#'
#' @param x An object of either class \code{prcomp} or class
#' \code{princomp}.
#'
#' @return A list with either classes \code{converted_from_princomp} and
#' \code{prcomp} or classes \code{converted_from_prcomp} and \code{princomp}.
#'
#' @template authors-BH
#'
#' @seealso \code{\link{prcomp}}, \code{\link{princomp}}
#' Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @keywords classes utilities
#'
#' @export
#' @noRd
#'
.q2rPCA <- function(x) {
  if (!inherits(x, "prcomp")) stop("The PCA object was not of class prcomp")

  # sdev, center and scale for both classes are the same; no change necessary
  # Other list elements carried along unchanged
  # Can fully pass as class princomp

  x$loadings <- x$rotation
  x$rotation <- NULL
  x$n.obs <- dim(x$x)[1]
  x$scores <- x$x
  x$x <- NULL
  x$call <- "No call available (data converted from class prcomp)"
  class(x) <- c("converted_from_prcomp", "princomp")
  x
}

#'
#' Identify Extreme Values in 3D
#'
#' A utility function to identify the extreme values in a 3D plot data set,
#' presumably so that they can be labeled. An internal function,
#' not generally called by the user.
#'
#' @param data A matrix of 3 columns containing x, y and z values for the
#' labels, with rows corresponding to sample names.
#'
#' @param names A character vector of sample names; must have length equal to
#' \code{nrow(data)}.
#'
#' @param tol A number describing the fraction of points to be labeled.
#' \code{tol = 1.0} labels all the points; \code{tol = 0.05} labels
#' \emph{approximately} the most extreme 5 percent.  Note that this is simply
#' based upon quantiles, assumes that x, y and z are each normally distributed,
#' and treats x, y and yz separately.  Thus, this is not a formal treatment of
#' outliers, just a means of labeling points.  Note too that while this
#' function could deal with groups separately, the way it is called by
#' \code{\link{plotScoresRGL}} lumps all groups together.
#'
#' @return A data frame containing the x, y and z coordinates, along with the
#' corresponding labels.
#'
#' @template authors-BH
#'
#' @seealso Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @keywords utilities
#'
#' @export
#' @noRd
#' @importFrom stats quantile
#'
.labelExtremes3d <- function(data, names, tol) {
  px <- data[, 1]
  py <- data[, 2]
  pz <- data[, 3]
  pl <- names
  if (is.numeric(pl)) pl <- format(pl, digits = 4)

  q.x <- quantile(px, probs = c(1.0 - tol, tol))
  sel.x <- (px <= q.x[2]) | (px >= q.x[1])
  keep.x <- subset(px, sel.x)
  keep.x <- match(keep.x, px) # need to keep this & corresponding y, z

  q.y <- quantile(py, probs = c(1.0 - tol, tol))
  sel.y <- (py <= q.y[2]) | (py >= q.y[1])
  keep.y <- subset(py, sel.y)
  keep.y <- match(keep.y, py) # need to keep this & corresponding x, z

  q.z <- quantile(pz, probs = c(1.0 - tol, tol))
  sel.z <- (pz <= q.z[2]) | (pz >= q.z[1])
  keep.z <- subset(pz, sel.z)
  keep.z <- match(keep.z, pz) # need to keep this & corresponding x, y

  keep <- unique(c(keep.x, keep.y, keep.z))

  x <- px[keep]
  y <- py[keep]
  z <- pz[keep]
  l <- pl[keep]

  ex.pts <- data.frame(x, y, z, l)
}

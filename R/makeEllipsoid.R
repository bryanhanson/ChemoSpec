#'
#' Create Ellipsoid
#'
#' Given at least 3 data points, this function creates either classical or
#' robust ellipsoids at a given confidence limit, in either 2D or 3D.  The
#' ellipsoids consist of randomly generated points, which if plotted as tiny
#' points, create a sort of transparent surface. An internal function, not
#' generally called by the user.
#'
#' If only x and y are provided, at least 3 points must be given, as 2 points
#' defines a line, not an ellipse.  For 3D data, and \code{rob = FALSE}, at
#' least 4 points must be provided.  If \code{rob = TRUE}, 5 points would be
#' theoretically required, but the code forces 8 to avoid unusual cases which
#' would fail.  If fewer than 8 are given, the computation shifts to classical
#' with a warning.  Note that depending upon how this function is called, one
#' may end up with classical and robust ellipsoids in the plot.  Remember too
#' that because the points are randomly generated, the x, y pairs or x, y, z
#' triplets are not related to each other, and one cannot plot lines from point
#' to point.  See the example for a 2D ellipse.  If you want a function that
#' generates x, y points suitable for connecting to each other via lines, see
#' \code{\link{plotScoresCor}}.
#'
#' @param data A matrix of at least 3 data points, with x, y and optionally z
#' in columns.  See details.
#'
#' @param cl The confidence limit desired.
#'
#' @param rob Logical, indicating if robust ellipsoids are to be computed.
#'
#' @param frac.pts.used If \code{rob = TRUE}, this is the fraction of points to
#' be considered the "good" part of the data.  See the documentation for
#' \code{\link{cov.rob}} for details.
#'
#' @return A matrix of 2 or 3 columns, representing x, y and optionally z.
#' These are the coordinates of points specifying an ellipse which has a
#' likelihood of containing the true mean at the given confidence limit.
#'
#' @note The idea was taken from "An Introduction to rggobi" found at the ggobi
#' web site (originally www.ggobi.org but not available as of June 2018).  I added the robust option.
#'
#' @template authors-BH
#'
#' @seealso \code{\link{cov.rob}} for the function that does the work.
#' Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @keywords multivariate utilities
#'
#' @examples
#'
#' # 2D example
#' x <- rnorm(10, 2, 0.5)
#' y <- rnorm(10, -2, 2)
#' ell <- .makeEllipsoid(cbind(x, y), cl = 0.99)
#' plot(ell[, 1], ell[, 2], col = "red", pch = 20, cex = 0.3)
#' points(x, y)
#'
#' @export
#' @noRd
#' @importFrom stats var rnorm qf
#'
.makeEllipsoid <- function(data, cl = 0.95, rob = FALSE, frac.pts.used = 0.80) {

  # This function generates a random set of points on a sphere, and turns it into an ellipsoid.
  # If rob = FALSE, it is shaped and sized to match the desired classical confidence level.
  # If rob = TRUE, a robust calculation is performed to produce the ellipsoid.
  # Basic idea came from an example on the ggobi web site; I added the robust option.

  # For 2D data, there must be at least 3 data points
  # For 3D data, there must be at least 4 data points for rob = FALSE
  # For 3D data, there must be at least 5 data points for rob = TRUE, generally,
  # but, we will throw an error if there are fewer than 8 points for a robust calculation

  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("You need to install package MASS to use this function")
  }
  npoints <- 1000 # number of points in the ellipse
  mean <- colMeans(data)
  p <- length(mean)
  n <- nrow(data)

  if ((p == 2) && (n < 3)) stop("You need at least 3 data points for 2D data")
  if ((p == 3) && (n < 4)) stop("You need at least 4 data points for 3D data")
  if ((p == 3) && (n < 5) && (rob)) stop("You need at least 5 data points for 3D data & robust computation")
  if ((p == 3) && (n < 8) && (rob)) {
    warning("Robust 3D calculations work best with at least 8 data points, switching to classical computation")
    rob <- FALSE
  }

  sphere <- matrix(rnorm(npoints * p), ncol = p)
  cntr <- t(apply(sphere, 1, .normVec)) # normalized sphere

  if (!rob) {
    cov <- var(data)
    ev <- eigen(cov)
    cntr <- cntr %*% diag(sqrt(ev$values)) %*% t(ev$vectors) # ellipsoid of correct shape
    Fcrit <- qf(cl, p, n - p)
    scalefactor <- sqrt(p * (n - 1) * Fcrit / (n * (n - p)))
    cntr <- cntr * scalefactor # ellipsoid of correct size
  }

  if (rob) {
    qt <- floor(n * frac.pts.used)
    robust <- MASS::cov.rob(data, method = "mcd", nsamp = "sample", quantile.used = qt)
    j <- length(robust$best)
    q <- robust$n.obs

    cov <- robust$cov
    ev <- eigen(cov)
    cntr <- cntr %*% diag(sqrt(ev$values)) %*% t(ev$vectors) # ellipsoid of correct shape
    Fcrit <- qf(cl, p, j - p)
    scalefactor <- sqrt(p * (j - 1) * Fcrit / (j * (j - p)))
    cntr <- cntr * scalefactor # ellipsoid of correct size
    mean <- robust$center # robust mean is different from classical mean
  }

  colnames(cntr) <- colnames(data)
  cntr + rep(mean, each = npoints) # locates ellipsoid properly
}

#'
#' Functions to Compute Measures of Central Tendency and Spread.  seX!
#'
#' These functions compute various measures of central tendency and spread.
#' These functions return a vector containing the measure of central tendency,
#' as well as that measure +/- the requested spread.  \code{.seX} is a little
#' different from the others in that it simply returns the standard error of x,
#' hence \code{.seX}.  Haven't we always needed a function for \code{.seX}?
#'
#' These functions employ \code{na.omit}.
#'
#' @param x A vector of numeric values whose measure of central tendency and
#' spread are to be computed.
#'
#' @return For all but \code{.seX}, a vector of 3 numeric values, giving the
#' measure of central tendency, that measure + the spread, and that measure -
#' the spread.
#' For \code{.seX}, a single value giving the standard error of x.
#'
#' @author `r .writeDoc_Authors("BH")
#'
#' @keywords internal
#'
#' @seealso Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @importFrom stats na.omit sd fivenum median
#' @noRd
#'
#' @examples
#'
#' x <- rnorm(100)
#' .seX(x)
#' .seXy(x)
#' .seXy95(x)
#' .seXyMad(x)
#' .seXyIqr(x)
#'
.seX <- function(x) sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))

.seXy <- function(x) {
  m <- mean(na.omit(x))
  se <- .seX(x)
  u <- m + se
  l <- m - se
  c(y = m, ymin = l, ymax = u)
}

.seXy95 <- function(x) {
  m <- mean(na.omit(x))
  se <- .seX(x)
  u <- m + 1.96 * se
  l <- m - 1.96 * se
  c(y = m, ymin = l, ymax = u)
}

.seXyIqr <- function(x) {
  i <- fivenum(x)
  c(y = i[3], ymin = i[2], ymax = i[4])
}

.seXyMad <- function(x) {
  m <- median(na.omit(x))
  d <- mad(na.omit(x))
  u <- m + d
  l <- m - d
  c(y = m, ymin = l, ymax = u)
}


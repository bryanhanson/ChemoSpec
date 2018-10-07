#'
#'
#' Functions to Compute Measures of Central Tendency and Spread.  seX!
#' 
#' These functions compute various measures of central tendency and spread.
#' These functions return a vector containing the measure of central tendency,
#' as well as that measure +/- the requested spread.  \code{.seX} is a little
#' different from the others in that it simply returns the standard error of x,
#' hence \code{.seX}.  Haven't we always needed a function for \code{.seX}?
#' 
#' These functions include \code{na.omit}.
#' 
#' @aliases .seXy .seX .seXyIqr .seXyMad .seXy95
#'
#' @param x A vector of numeric values whose measure of central tendency and
#' spread are to be computed.
#'
#' @return For all but \code{.seX}, a vector of 3 numeric values, giving the
#' measure of central tendency, that measure + the spread, and that measure -
#' the spread.
#' For \code{.seX}, a single value giving the standard error of x.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#'
#' @keywords utilities
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
#' @export .seX .seXy .seXy95 .seXyMad .seXyIqr
#'
#' @importFrom stats na.omit sd fivenum median
#'

#' @describeIn .seXy standard error of x

.seX <- function(x) sd(x, na.rm = TRUE)/sqrt(length(na.omit(x)))

#' @describeIn .seXy mean +/- the standard error

.seXy <- function(x) {
	m <- mean(na.omit(x))
	se <- .seX(x)
	u <- m + se
	l <- m - se
	c(y = m, ymin = l, ymax = u)
	}

#' @describeIn .seXy mean +/- the standard error at 95\% conf. interval
.seXy95 <- function(x) {
	m <- mean(na.omit(x))
	se <- .seX(x)
	u <- m + 1.96 * se
	l <- m - 1.96 * se
	c(y = m, ymin = l, ymax = u)
	}


#' @describeIn .seXy median +/- the 1st and 3rd quantile
.seXyIqr <- function(x) {
	i <- fivenum(x)
	c(y = i[3], ymin = i[2], ymax = i[4])
	}
	

#' @describeIn .seXy median +/- median absolute deviation
.seXyMad <- function(x) {
	m <- median(na.omit(x))
	d <- mad(na.omit(x))
	u <- m + d
	l <- m - d
	c(y = m, ymin = l, ymax = u)
	}

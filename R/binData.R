#'
#' Bin or Bucket Data
#'
#' This function accepts a vector of x-values and averages them in groups of
#' \code{bin.ratio} data points.  It also accepts a vector of y-values and sums
#' them in groups of \code{bin.ratio} data points.  Both x and y data can be
#' processed in the same call, or they can be processed separately.  An
#' internal function, not generally called by the user.
#'
#' The x and y values must be contiguous in the sense that there are no gaps in
#' the values (i.e., x[n + 1] - x[n] must be the same for the entire data set;
#' this can be checked with \code{\link{diff}} and is checked internally.  Note
#' that this function is normally called by \code{\link{binSpectra}} and that
#' function can handle gaps, sending each continuous piece of data here to be
#' binned.  If length(x or y) is not divisible by bin.ratio to give a whole
#' number, data points are removed from the beginning of x or y until it is,
#' and the number of data points removed is reported at the console.  The
#' algorithm forces the requested bin.ratio to be used.
#'
#' @param x An optional vector of x values to be averaged in groups of
#' \code{bin.data} points.
#'
#' @param y An optional vector of y values to be summed in groups of
#' \code{bin.data} points.
#'
#' @param bin.ratio An integer giving the binning ratio, that is, the number of
#' points to be grouped together into one subset of data.
#'
#' @return Depending upon the input, a data frame containing one or both of
#' the following elements:
#' \item{mean.x}{A vector of the averaged x values.  Length will be
#' approximately length(x)/bin.ratio, with length(x) adjusted as described
#' above if this does not give a whole number.} \item{sum.y}{A vector of the
#' summed y values.  Length will be approximately length(y)/bin.ratio, with
#' length(y) adjusted as described above if this does not give a whole number.}
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords utilities manip
#'
#' @examples
#'
#' x <- seq(0, 1000, length.out = 3000)
#' y <- rnorm(3000)
#' res <- .binData(x, y)
#' length(res$mean.x) # will be half of the original length
#'
#' # Now try it with bin.ratio that does not divide into 3000
#' res <- binData(x, y, bin.ratio = 7)
#' length(res$mean.x)
#' @export
#' @noRd
#'
.binData <- function(x = NULL, y = NULL, bin.ratio = 2) {

  # Be careful:
  # bin.ratio may not divide evenly into no. data points
  # Drop a few data points on one end so that it does

  if (bin.ratio <= 1) stop("bin.ratio must > 1")
  if (!.isWholeNo(bin.ratio)) stop("bin.ratio must be an integer > 1")
  if (!is.null(y) && !is.null(x)) {
    if (!identical(length(x), length(y))) stop("x and y vectors in binData have different lengths")
  }

  chk <- check4Gaps(x, silent = TRUE)
  if (nrow(chk) > 1) stop("The data being binned has gaps and cannot be binned accurately")
  br <- bin.ratio

  if (!is.null(x)) len <- length(x)
  if (!is.null(y)) len <- length(y)
  no.bins <- len / br # initial value; maybe final too
  if (!.isWholeNo(no.bins)) { # trim data just a bit so no.bins is a whole number
    chop <- NULL
    n <- 0
    while (n < br) {
      n <- n + 1
      l <- len - n
      no.b <- l / br
      if (.isWholeNo(no.b)) {
        chop <- n
        break
      }
    }
    rem <- c(1:chop) # chop off the first few data points
    if (!is.null(x)) x <- x[-rem]
    if (!is.null(y)) y <- y[-rem]
    if (!is.null(x)) len <- length(x)
    if (!is.null(y)) len <- length(y)
    no.bins <- len / br # reset these values
  }

  # Using data grouped in sets of bin.ratio length, avg the x values and sum the y-values
  # Three cases possible: only x is provided, only y is provided, both are provided

  b.x <- c(rep(NA, no.bins))
  b.y <- c(rep(NA, no.bins))

  cnt <- seq(1, len, br) # length of cnt will = no.bins
  inc <- br - 1

  if (!is.null(x)) {
    for (n in 1:no.bins) {
      r <- c(cnt[n]:(cnt[n] + inc))
      b.x[n] <- mean(x[r])
    }
  }

  if (!is.null(y)) {
    for (n in 1:no.bins) {
      r <- c(cnt[n]:(cnt[n] + inc))
      b.y[n] <- sum(y[r])
    }
  }

  if (!is.null(y)) res <- data.frame(sum.y = b.y)
  if (!is.null(x)) res <- data.frame(mean.x = b.x)
  if (!is.null(y) && !is.null(x)) res <- data.frame(mean.x = b.x, sum.y = b.y)
  res
}

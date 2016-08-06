#'
#"
#' Compute Confidence Ellipses
#' 
#' A utility function which when given a x,y data set computes both classical
#' and robust confidence ellipses. An internal function, not generally called
#' by the user.
#' 
#' 
#' @param x As per \code{\link[mvoutlier]{cor.plot}}.
#'
#' @param quan As per \code{\link[mvoutlier]{cor.plot}}.
#'
#' @param alpha As per \code{\link[mvoutlier]{cor.plot}}.
#'
#' @return A list with the following elements (a simpler version of that in the
#' original function \code{\link[mvoutlier]{cor.plot}}): \item{x.cls}{The x
#' values for the classical ellipse.} \item{y.cls}{The y values for the
#' classical ellipse.} \item{c}{The correlation value for the classical
#' ellipse.} \item{x.rob}{The x values for the robust ellipse.}
#' \item{y.rob}{The y values for the robust ellipse.} \item{r}{The correlation
#' value for the robust ellipse.}
#'
#' @author Bryan A. Hanson, DePauw University. Derived from \code{\link[mvoutlier]{cor.plot}}.
#'
#' @seealso See function \code{\link[mvoutlier]{cor.plot}} in package
#' \pkg{mvoutlier} on which this function is based.
#'
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#'
#' @keywords multivariate
#'
#' @export plotScoresCor
#'
#' @importFrom stats cov cor qchisq
# @importFrom robustbase covMcd
#'
plotScoresCor <- function (x, quan = 1/2, alpha = 0.025) {

# Function to plot robust correlation ellipsoids for x, y data
# Modifed slightly from cor.plot {mvoutlier}
# Part of the ChemoSpec Package
# Bryan Hanson, DePauw University, Aug 2009

	if (!requireNamespace("robustbase", quietly = TRUE)) {
		stop("You need to install package robustbase to use this function")
	}

	x <- as.matrix(x)
    covr <- robustbase::covMcd(x, cor = TRUE, alpha = quan)
    cov.svd <- svd(cov(x), nv = 0)
    covr.svd <- svd(covr$cov, nv = 0)
    r <- cov.svd[["u"]] %*% diag(sqrt(cov.svd[["d"]]))
    rr <- covr.svd[["u"]] %*% diag(sqrt(covr.svd[["d"]]))
    e <- cbind(cos(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1 - alpha, 
        2)), sin(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1 - alpha, 
        2)))
    tt <- t(r %*% t(e)) + rep(1, 101) %o% apply(x, 2, mean)
    ttr <- t(rr %*% t(e)) + rep(1, 101) %o% covr$center
    
   	ellipse <- list()
   	ellipse$x.cls <- tt[,1]
   	ellipse$y.cls <- tt[,2]
   	ellipse$c <- round(cor(x)[1, 2], 2)
   	ellipse$x.rob <- ttr[,1]
   	ellipse$y.rob <- ttr[,2]
   	ellipse$r <- round(covr$cor[1, 2], 2)
   	ellipse

 }


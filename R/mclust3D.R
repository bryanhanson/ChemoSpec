#'
#'
#' mclust Analysis in 3D
#' 
#' This function conducts an mclust analysis of the data provided, and plots
#' the points in 3D using \pkg{rgl} graphics.  An option is provided for displaying
#' either classical or robust confidence ellipses.  An internal function not
#' generally called by the user.  See \code{\link{mclust3dSpectra}} instead.
#' 
#' 
#' @param data A matrix of 3 columns (corresponding to x, y, z) and samples in
#' rows.
#' @param ellipse Logical indicating if confidence ellipses should be drawn.
#'
#' @param rob Logical; if \code{ellipse = TRUE}, indicates that robust
#' confidence ellipses should be drawn.  If \code{FALSE}, classical confidence
#' ellipses are drawn.
#' @param cl A number indicating the confidence interval for the ellipse.
#'
#' @param frac.pts.used If \code{ellipse = TRUE} and \code{rob = TRUE}, a
#' number indicating the fraction of the data points to be considered "good"
#' and thus used to compute the robust confidence ellipse.
#'
#' @param truth A character vector indicating the known group membership for
#' reach row of the PC scores.  Generally this would be \code{spectra$groups}.
#'
#' @param title A character string for the plot title.
#'
#' @param t.pos A character selection from \code{LETTERS[1:8]} ( = A through H)
#' indicating the desired location for the title.
#'
#' @param lab.opts A logical indicating whether or not to display the locations
#' where the title and legend can be placed.  These locations are the corners
#' of a cube surrounding the data.
#'
#' @param use.sym logical; if true, the color scheme is changed to black and
#' symbols are used for plotting.
#'
#' @param \dots Other parameters to be passed downstream.
#'
#' @return The mclust model is returned invisibly, and a plot is produced.
#'
#' @seealso \code{\link[mclust]{Mclust}} for background on the method.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#'
#' @keywords multivariate cluster
#'
#' @examples
#' 
#' \dontrun{
#'
#' require("mclust")
#' set.seed(666)
#' x <- c(rnorm(10, 3, 0.5), rnorm(10, -1, 0.5))
#' y <- c(rnorm(10, 1, 1), rnorm(10, -4, 0.5))
#' z <- c(rnorm(10, -2, 0.5), rnorm(10, 3, 0.5))
#' x[15] <- y[15] <- z[15] <- 4 # screw up one point
#' my.truth <- c(rep("Z", 10), rep("Q", 10))
#' mclust3D(cbind(x, y, z), title = "mclust3D demo",
#' 	 t.pos = "G", truth = my.truth)
#' }
#' 
#' @export mclust3D
#'
# @importFrom rgl open3d segments3d text3d points3d
# @importFrom mclust Mclust classError
# @importFrom RColorBrewer brewer.pal
#'
mclust3D <- function(data, ellipse = TRUE, rob = FALSE, 
	cl = 0.95, frac.pts.used = 0.8, truth = NULL,
	title = "no title provided", t.pos = NULL, lab.opts = FALSE,
	use.sym = FALSE, ...) {

# Function to plot mclust results in 3D with confidence ellipses
# Bryan Hanson, DePauw University, Dec 2009
# This is the "plain" version that works w/o ChemoSpec package

	if (!requireNamespace("mclust", quietly = TRUE)) {
		stop("You need to install package mclust to use this function")
	}

	if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
		stop("You need to install package RColorBrewer or supply the colors yourself")
		}

	if (!dim(data)[2] == 3) stop("Data must contain 3 columns (x, y, z)")

	mod <- mclust::Mclust(data, ...)
	gr <- unique(mod$classification)
	my.col <- c("red", "blue")
	if (length(gr) > 2) my.col <- RColorBrewer::brewer.pal(length(gr), "Set1")
	my.sym <- letters[1:length(gr)]
	df1 <- df2 <- data.frame(x = NA, y = NA, z = NA, sym = NA, col = NA, a = NA)

	for (n in 1:length(gr)) { # first gather the original data points
		w <- grep(gr[n], mod$classification)
		x <- data[w,1] 					
		y <- data[w,2]
		z <- data[w,3]
		sym <- rep(my.sym[n], length(w))
		col <- rep(my.col[n], length(w))
		a <- rep(1, length(w))
		temp <- data.frame(x = x, y = y, z = z, sym = sym, col = col, a = a)
		df1 <- rbind(df1, temp)

		if ((length(w)) > 3 && (ellipse)) { # now gather the ellipsoid points
			ell <- makeEllipsoid(data[w,], rob = rob, ...)
			x <- ell[,1]
			y <- ell[,2]
			z <- ell[,3]
			sym <- rep(my.sym[n], 1000)
			col <- rep(my.col[n], 1000)
			a <- rep(0.1, 1000)
			temp <- data.frame(x = x, y = y, z = z, sym = sym, col = col, a = a)
			df2 <- rbind(df2, temp)
			}
	}
	
	df1 <- df1[-1,]
	if (ellipse) df2 <- df2[-1,]
	a <- range(df1$x, df1$y, df1$z) # scale the axis to the data
	b <- abs(a[1])
	d <- abs(a[2])
	ax.len <- max(a, b)
	x.cor <- c(0, ax.len, 0, 0) 
	y.cor <- c(0, 0, ax.len, 0)
	z.cor <- c(0, 0, 0, ax.len)
	i <- c(1, 2, 1, 3, 1, 4)

	pos <- matrix(NA, 8, 3) # set up cube corner labels
	pos[1:4,1] <- ax.len # x values
	pos[5:8,1] <- -ax.len
	pos[c(1,2,7,8), 2] <- ax.len # y values
	pos[3:6, 2] <- -ax.len
	pos[seq(1,8,2), 3] <- ax.len # z values
	pos[seq(2,8,2), 3] <- -ax.len

	rgl::open3d() # draw axes, then label them
	rgl::segments3d(x.cor[i], y.cor[i], z.cor[i], lwd = 2.0,
		line_antialias = TRUE)
	rgl::text3d(x.cor * 1.1, y.cor * 1.1, z.cor * 1.1, texts = c("", "X", "Y", "Z"),
		adj = c(0, 1))

	if (lab.opts) { # plot letters in the cube corners
		labs <- LETTERS[1:8]
		if (!use.sym) rgl::text3d(pos, texts = labs, col = "orange")
		if (use.sym) rgl::text3d(pos, texts = labs, col = "black")
		}

	if ((!is.null(title)) && (!is.null(t.pos))) { # plot title
		m <- match(t.pos, LETTERS[1:8])
		rgl::text3d(pos[m,], texts = title, adj = c(0.5, 0.5), cex = 1.5)
		}


	if (use.sym) df1$col <- df2$col <- "black"
	if (ellipse) rgl::points3d(df2$x, df2$y, df2$z, alpha = df2$a, size = 4, color = df2$col, 
		point_antialias = TRUE) # draw ellipsoids
	rgl::text3d(df1$x, df1$y, df1$z, texts = df1$sym, color = df1$col) # draw original points

	if (!is.null(truth)) { # X out errors in classification
		ans <- mclust::classError(mod$classification, truth)
		wh <- data[ans$misclassified,]
		if (length(wh) == 0) warning("No points were misclassified, damn you're good!")
		if (length(wh) > 0) rgl::text3d(wh, texts = "X", color = "black", cex = 1.5)
		}

	invisible(mod)
	}

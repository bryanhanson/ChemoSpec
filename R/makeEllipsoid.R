makeEllipsoid <-
function(data, cl = 0.95, rob = FALSE, frac.pts.used = 0.80) {

# Function to make an ellipsoid surrounding a given set of 2 or 3D data
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Dec 2009

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
		rob = FALSE
		}

	sphere <- matrix(rnorm(npoints*p), ncol = p)
	cntr <- t(apply(sphere, 1, normVec))  # normalized sphere
	
	if (!rob) {
		cov <- var(data)
		ev <- eigen(cov)
		cntr <- cntr %*% diag(sqrt(ev$values)) %*% t(ev$vectors) # ellipsoid of correct shape
		Fcrit <- qf(cl, p, n-p)
		scalefactor <- sqrt(p*(n-1)*Fcrit/(n*(n-p)))
		cntr <- cntr*scalefactor # ellipsoid of correct size
		}	
	
	if (rob) {
		qt <- floor(n * frac.pts.used)
		robust <- MASS::cov.rob(data, method = "mcd", nsamp = "sample", quantile.used = qt)
		j <- length(robust$best)
		q <- robust$n.obs

		cov <- robust$cov
		ev <- eigen(cov)	
		cntr <- cntr %*% diag(sqrt(ev$values)) %*% t(ev$vectors) # ellipsoid of correct shape
		Fcrit <- qf(cl, p, j-p)
		scalefactor <- sqrt(p*(j-1)*Fcrit/(j*(j-p)))
		cntr <- cntr*scalefactor # ellipsoid of correct size
		mean <- robust$center # robust mean is different from classical mean
		}
		
		colnames(cntr) <- colnames(data)
		cntr + rep(mean, each = npoints) # locates ellipsoid properly

		}

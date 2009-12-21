labelExtremes3d <-
function(data, names, tol) {

# Function to find extreme data points for labeling
# Part of the ChemoSpec package
# Bryan Hanson, DePauw Univ, Dec 2009
	
	px <- data[,1]
	py <- data[,2]
	pz <- data[,3]
	pl <- names
	if (is.numeric(pl)) pl <- format(pl, digits = 4)
		
	q.x <- quantile(px, probs = c(1.0-tol, tol))
	sel.x <- (px <= q.x[2]) | (px >= q.x[1])
	keep.x <- subset(px, sel.x)
	keep.x <- match(keep.x, px) # need to keep this & corresponding y, z
		
	q.y <- quantile(py, probs = c(1.0-tol, tol))
	sel.y <- (py <= q.y[2]) | (py >= q.y[1])
	keep.y <- subset(py, sel.y)
	keep.y <- match(keep.y, py) # need to keep this & corresponding x, z
		
	q.z <- quantile(pz, probs = c(1.0-tol, tol))
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


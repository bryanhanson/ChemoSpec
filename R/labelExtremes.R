labelExtremes <-
function(data, names, tol) {

# Function to label extreme data points
# Part of the ChemoSpec package
# Bryan Hanson, DePauw Univ, Aug 2009
	
	px <- data[,1]
	py <- data[,2]
	pl <- names
	if (is.numeric(pl)) pl <- sprintf("%.2f", pl)
		
#	q.x <- quantile(px, probs = c(1.0-tol, tol)) # also below
	q.x <- quantile(px, probs = c(1.0-tol, tol), na.rm = TRUE)
	sel.x <- (px <= q.x[2]) | (px >= q.x[1])
	keep.x <- subset(px, sel.x)
	keep.x <- match(keep.x, px) # need to keep this & corresponding y
		
	q.y <- quantile(py, probs = c(1.0-tol, tol), na.rm = TRUE)
	sel.y <- (py <= q.y[2]) | (py >= q.y[1])
	keep.y <- subset(py, sel.y)
	keep.y <- match(keep.y, py) # need to keep this & corresponding x
		
	keep <- unique(c(keep.x, keep.y))

	x <- px[keep]
	y <- py[keep]
	l <- pl[keep]

	for(n in c(1:length(x))) {
		text(x[n], y[n], l[n], pos = 4, offset = 0.2, cex = 0.5)
		}
			
	}


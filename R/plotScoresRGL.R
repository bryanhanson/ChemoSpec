
plotScoresRGL <-
function(spectra, pca, pcs = c(1:3), title = NULL,
	t.pos = NULL, leg.pos = NULL, lab.opts = FALSE, ...) {

# Function to do an interactive 3D plot of PCA scores using rgl
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Nov 2009

	if (!length(pcs) == 3) stop("Please give exactly 3 PCs to plot")
	chkSpectra(spectra)

	x <- pca$x[, pcs[1]]
	y <- pca$x[, pcs[2]]
	z <- pca$x[, pcs[3]]
	colors <- spectra$colors
	
	eigensum <- sum(pca$sdev*pca$sdev) # prepare axis labels
	variance <- 100*(pca$sdev*pca$sdev/eigensum)
	x.lab <- paste("PC", 1, " (", format(variance[1], digits=2), "%", ")", sep = "")
	y.lab <- paste("PC", 2, " (", format(variance[2], digits=2), "%", ")", sep = "")
	z.lab <- paste("PC", 3, " (", format(variance[3], digits=2), "%", ")", sep = "")
		
	a <- range(x, y, z) # scale the axis to the data
	b <- abs(a[1])
	d <- abs(a[2])
	ax.len <- max(a, b)
	x.cor <- c(0, ax.len, 0, 0) 
	y.cor <- c(0, 0, ax.len, 0)
	z.cor <- c(0, 0, 0, ax.len)
	i <- c(1, 2, 1, 3, 1, 4)
		
	open3d() # taken from the example page for rgl
	segments3d(x.cor[i], y.cor[i], z.cor[i], lwd = 2.0, line_antialias = TRUE) # draw axes, next label axes
	text3d(x.cor * 1.1, y.cor * 1.1, z.cor * 1.1, texts = c("", x.lab, y.lab, z.lab), adj = c(0, 1))
	points3d(x, y, z, col = colors, size = 4, point_antialias = TRUE) # draw data

	# prep matrix giving the 8 corners of the cube for labeling purposes
	
	pos <- matrix(NA, 8, 3)
	pos[1:4,1] <- ax.len # x values
	pos[5:8,1] <- -ax.len
	pos[c(1,2,7,8), 2] <- ax.len # y values
	pos[3:6, 2] <- -ax.len
	pos[seq(1,8,2), 3] <- ax.len # z values
	pos[seq(2,8,2), 3] <- -ax.len

	if (lab.opts) {
		labs <- LETTERS[1:8]
		text3d(pos, texts = labs, col = "orange")
		}

	if ((!is.null(title)) && (!is.null(t.pos))) {
		m <- match(t.pos, LETTERS[1:8])
		text3d(pos[m,], texts = title, adj = c(0.5, 0.5), cex = 1.5)
		}
	
	if (!is.null(leg.pos)) {
		m <- match(leg.pos, LETTERS[1:8])
		h <- length(levels(spectra$groups))
		
		sop <- matrix(NA, h, 3) # offset labels into 2 cols
		sop[,2] <- pos[m,2]  # keep y coord constant
		amt <- 0.2 * ax.len
		od <- seq(1, h, 2)
		ev <- seq(2, h, 2) # shift every other row /r
		for (n in od) sop[n,1] <- pos[m,1]-amt
		for (n in ev) sop[n,1] <- pos[m,1]+amt	
		
		# fix z coord - a bit trickier!
		r <- ceiling(h/2) # number of rows
		z.off <- seq(-0.5 * (r -1), 0.5 * (r - 1), 1)
		z.off <- sort(rep(z.off, 2), decreasing = TRUE)
		z.off <- z.off[1:h] # truncate so dim's match if h odd
		z.off <- z.off * amt + pos[m,3] # center around specified point
		sop[,3] <- z.off
#		print(sop)
		text3d(sop, texts = levels(spectra$groups),
			adj = c(0.5, 0.5), col = levels(as.factor(spectra$colors)))
		
		}	
	}

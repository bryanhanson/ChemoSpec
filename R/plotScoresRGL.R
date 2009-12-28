
plotScoresRGL <- function(spectra, pca, pcs = c(1:3), 
	ellipse = TRUE, rob = FALSE, cl = 0.95, frac.pts.used = 0.8,
	title = NULL, t.pos = NULL, leg.pos = NULL, lab.opts = FALSE,
	tol = 0.01, use.sym = FALSE,...) {

# Function to do an interactive 3D plot of PCA scores using rgl
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Dec 2009

	if (!length(pcs) == 3) stop("Please give exactly 3 PCs to plot")
	chkSpectra(spectra)

	x <- pca$x[, pcs[1]]
	y <- pca$x[, pcs[2]]
	z <- pca$x[, pcs[3]]
	colors <- spectra$colors
	lets <- spectra$alt.sym
	gr <- sumGroups(spectra)
	
	eigensum <- sum(pca$sdev*pca$sdev) # prepare axis labels
	variance <- 100*(pca$sdev*pca$sdev/eigensum)
	x.lab <- paste("PC", pcs[1], " (", format(variance[1], digits=2), "%", ")", sep = "")
	y.lab <- paste("PC", pcs[2], " (", format(variance[2], digits=2), "%", ")", sep = "")
	z.lab <- paste("PC", pcs[3], " (", format(variance[3], digits=2), "%", ")", sep = "")
		
	a <- range(x, y, z) # scale the axis to the data
	b <- abs(a[1])
	d <- abs(a[2])
	ax.len <- max(a, b)
	x.cor <- c(0, ax.len, 0, 0) 
	y.cor <- c(0, 0, ax.len, 0)
	z.cor <- c(0, 0, 0, ax.len)
	i <- c(1, 2, 1, 3, 1, 4)
		
	open3d() # draw axes and label them
	segments3d(x.cor[i], y.cor[i], z.cor[i], lwd = 2.0,
		line_antialias = TRUE)
	text3d(x.cor * 1.1, y.cor * 1.1, z.cor * 1.1,
		texts = c("", x.lab, y.lab, z.lab), adj = c(0, 1))
	if (!use.sym) points3d(x, y, z, col = colors, size = 4, point_antialias = TRUE)
	if (use.sym) text3d(x, y, z, texts = lets) # draw data points

	if (tol > 0) {
		pts <- labelExtremes3d(pca$x[,pcs], names = spectra$names, tol = tol)
		text3d(pts[,1], pts[,2], pts[,3], texts = pts[,4], cex = 0.75, adj = c(0,0))
		}

	if (ellipse) { # compute and draw ellipsoids
		for (n in 1:length(gr$group)) {
			if (gr$no.[n] > 3) {
				d <- pca$x[,pcs]
				w <- grep(gr$group[n], spectra$groups)
				d <- d[w,]
				ell <- makeEllipsoid(d, rob = rob, cl = cl, frac.pts.used = frac.pts.used, ...)
				x <- ell[,1]
				y <- ell[,2]
				z <- ell[,3]
				col <- rep(gr$color[n], 1000)
				a <- rep(0.1, 1000)
				if (!use.sym) points3d(x, y, z, col = col, size = 4, alpha = a, point_antialias = TRUE)
				if (use.sym) points3d(x, y, z, col = "black", size = 4, alpha = a, point_antialias = TRUE)
					}
			}
		}

	pos <- matrix(NA, 8, 3) # matrix of label positions
	pos[1:4,1] <- ax.len # x values
	pos[5:8,1] <- -ax.len
	pos[c(1,2,7,8), 2] <- ax.len # y values
	pos[3:6, 2] <- -ax.len
	pos[seq(1,8,2), 3] <- ax.len # z values
	pos[seq(2,8,2), 3] <- -ax.len

	if (lab.opts) {  # label the 8 cube corners
		labs <- LETTERS[1:8]
		if (!use.sym) text3d(pos, texts = labs, col = "orange")
		if (use.sym) text3d(pos, texts = labs, col = "black")
		}

	if ((!is.null(title)) && (!is.null(t.pos))) { # add title
		m <- match(t.pos, LETTERS[1:8])
		text3d(pos[m,], texts = title, adj = c(0.5, 0.5), cex = 1.5)
		}
	
	if (!is.null(leg.pos)) { # add legend
		m <- match(leg.pos, LETTERS[1:8])
		h <- length(gr$group)
		
		sop <- matrix(NA, h, 3) # offset labels into 2 cols
		sop[,2] <- pos[m,2]  # keep y coord constant
		amt <- 0.2 * ax.len
		od <- seq(1, h, 2)
		for (n in od) sop[n,1] <- pos[m,1]-amt
		if (h > 1) {
			ev <- seq(2, h, 2) # shift every other row l/r
			for (n in ev) sop[n,1] <- pos[m,1]+amt			}
		
		# fix z coord - a bit trickier!
		r <- ceiling(h/2) # number of rows
		z.off <- seq(-0.5 * (r -1), 0.5 * (r - 1), 1)
		z.off <- sort(rep(z.off, 2), decreasing = TRUE)
		z.off <- z.off[1:h] # truncate so dim's match if h odd
		z.off <- z.off * amt + pos[m,3] # center around specified point
		sop[,3] <- z.off
		if (!use.sym) { text3d(sop, texts = gr$group,
			adj = c(0.5, 0.5), col = gr$color) }
			
		if (use.sym) {
			ss <- rbind(sop, sop) # double sop for label locs
			od <- seq(h+1, h*2, 2) # shift every other row l/r
			for (n in od) ss[n,1] <- ss[n,1]-amt
			if (h > 1)	{
				ev <- seq(h+2, h*2, 2) # push loc symbol farther out
				for (n in ev) ss[n,1] <- ss[n,1]+amt
				}
			text3d(ss, texts = c(gr$group, gr$alt.sym),
				adj = c(0.5, 0.5), col = "black")
			
			}
		
		}	
	}

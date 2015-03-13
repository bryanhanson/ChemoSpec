
plotScores3D <-
function(spectra, pca, pcs = c(1:3), ellipse = TRUE, rob = FALSE,
	cl = 0.95, frac.pts.used = 0.8,
	view = list(y = 34, x = 10, z = 0), tol = 0.01, use.sym = FALSE, ...) {

# Function to do a 3D plot of PCA scores
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Sept 2009

	if (!requireNamespace("lattice", quietly = TRUE)) {
		stop("You need to install package lattice to use this function")
	}

	if (!requireNamespace("grid", quietly = TRUE)) {
		stop("You need to install package grid to use this function")
	}

	if (!length(pcs) == 3) stop("Please give exactly 3 PCs to plot")
	chkSpectra(spectra)

	x <- pca$x[, pcs[1]] # organize a few things
	y <- pca$x[, pcs[2]]
	z <- pca$x[, pcs[3]]
	pch <- 20
	gr <- sumGroups(spectra)
	# move the original data into a df, add a size parameter
	df <- data.frame(x = x, y = y, z = z, sym = spectra$sym, col = spectra$colors, size = rep(0.5, length(spectra$names)))

	eigensum <- sum(pca$sdev*pca$sdev) # prepare axis labels
	variance <- 100*(pca$sdev*pca$sdev/eigensum)
	x.lab <- paste("PC", pcs[1], " (", format(variance[1], digits=2), "%", ")", sep = "")
	y.lab <- paste("PC", pcs[2], " (", format(variance[2], digits=2), "%", ")", sep = "")
	z.lab <- paste("PC", pcs[3], " (", format(variance[3], digits=2), "%", ")", sep = "")
	
	for (n in 1:length(gr$group)) { # work through the groups, add ellipses if n > 3
		# note that makeEllipsoid has further checks for the number of data points

		w <- grep(gr$group[n], spectra$groups)
		if ((gr$no.[n] > 3) && (ellipse)) { # gather the ellipsoid points
			ell <- makeEllipsoid(pca$x[w,pcs], rob = rob, cl = cl, frac.pts.used = frac.pts.used)
			x <- ell[,1]
			y <- ell[,2]
			z <- ell[,3]
			sym <- rep(gr$symbol[n], 1000)
			col <- rep(gr$color[n], 1000)
			size <- rep(0.05, 1000) # tiny points for a transparent ellipse
			temp <- data.frame(x = x, y = y, z = z, sym = sym, col = col, size = size)
			df <- rbind(df, temp)
			}
	}

	cube.key <- list(x = 0.5, y = 0.15, corner = c(0.5, 0.5), columns = length(gr$group),
		text = list(gr$group, col = gr$color, pch = 20))

	if (use.sym) { # need to override a few things
		df$col <- "black"
		pch <- df$sym
		cube.key <- list(x = 0.5, y = 0.15, corner = c(0.5, 0.5),
			columns = length(gr$group),
			text = list(gr$group, col = "black"), points = TRUE, pch = gr$sym)
		}


	p <- lattice::cloud(z ~ x * y, data = df, col = as.character(df$col), cex = df$size, pch = pch,
		xlab = x.lab, ylab = y.lab, zlab = z.lab, ...,
		par.settings = list(axis.line = list(col = "transparent"),
			par.xlab.text = list(cex = 0.75),
			par.ylab.text = list(cex = 0.75),
			par.zlab.text = list(cex = 0.75)),
		screen = view, zoom = 0.75,
		key = cube.key,
		panel = function(...) {
			lattice::panel.cloud(...)
#			if (tol > 0) {
#				pts <- labelExtremes3d(pca$x[,pcs], names = spectra$names, tol = tol)
#				labs <- as.character(pts[,4])
#				pts <- t(pts[,1:3])
#				#print(pts)
#				#print(labs)
#				chk1 <<- pts
#				panel.3d.text(xyz.in.row = pts, labels = labs, ...)
#				}
			}
		)

	plot(p)
	grid::grid.text(spectra$desc, 0.5, 0.1, gp = grid::gpar(fontsize = 10))

	}

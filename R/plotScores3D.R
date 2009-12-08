
plotScores3D <-
function(spectra, pca, pcs = c(1:3), title = "no title provided",
	view = list(y = 34, x = 10, z = 0), use.sym = FALSE, ...) {

# Function to do a simple 3D plot of PCA scores
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Sept 2009

	if (!length(pcs) == 3) stop("Please give exactly 3 PCs to plot")
	chkSpectra(spectra)

	x <- pca$x[, pcs[1]]
	y <- pca$x[, pcs[2]]
	z <- pca$x[, pcs[3]]
	colors <- spectra$colors
	my.pch <- 20
	
	eigensum <- sum(pca$sdev*pca$sdev) # prepare axis labels
	variance <- 100*(pca$sdev*pca$sdev/eigensum)
	x.lab <- paste("PC", 1, " (", format(variance[1], digits=2), "%", ")", sep = "")
	y.lab <- paste("PC", 2, " (", format(variance[2], digits=2), "%", ")", sep = "")
	z.lab <- paste("PC", 3, " (", format(variance[3], digits=2), "%", ")", sep = "")
	
	title <- paste(title, ": PCA Score Plot", sep = "")
	
	gr <- sumGroups(spectra)
	
	cube.key <- list(x = 0.5, y = 0.15, corner = c(0.5, 0.5), columns = length(gr$group),
		text = list(gr$group, col = gr$color, pch = 20))

	if (use.sym) { # need to override a few things
		colors <- "black"
		my.pch <- spectra$sym
		cube.key <- list(x = 0.5, y = 0.15, corner = c(0.5, 0.5),
			columns = length(gr$group),
			text = list(gr$group, col = "black"), points = TRUE, pch = gr$sym)
		}

	p <- cloud(z ~ x * y, col = colors,
		xlab = x.lab, ylab = y.lab, zlab = z.lab,
		xlim = 1.1*range(x), ylim = 1.1*range(y), zlim = 1.1*range(z),
		pch = my.pch, main = title,
		par.settings = list(axis.line = list(col = "transparent"),
		par.xlab.text = list(cex = 0.75),
		par.ylab.text = list(cex = 0.75),
		par.zlab.text = list(cex = 0.75)),
		screen = view, zoom = 0.75,
		key = cube.key)

	plot(p)
	grid.text(spectra$desc, 0.5, 0.1, gp = gpar(fontsize = 10))

	}

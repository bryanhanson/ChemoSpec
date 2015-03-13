
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

	open3d() # draw axes, then label them
	segments3d(x.cor[i], y.cor[i], z.cor[i], lwd = 2.0,
		line_antialias = TRUE)
	text3d(x.cor * 1.1, y.cor * 1.1, z.cor * 1.1, texts = c("", "X", "Y", "Z"),
		adj = c(0, 1))

	if (lab.opts) { # plot letters in the cube corners
		labs <- LETTERS[1:8]
		if (!use.sym) text3d(pos, texts = labs, col = "orange")
		if (use.sym) text3d(pos, texts = labs, col = "black")
		}

	if ((!is.null(title)) && (!is.null(t.pos))) { # plot title
		m <- match(t.pos, LETTERS[1:8])
		text3d(pos[m,], texts = title, adj = c(0.5, 0.5), cex = 1.5)
		}


	if (use.sym) df1$col <- df2$col <- "black"
	if (ellipse) points3d(df2$x, df2$y, df2$z, alpha = df2$a, size = 4, color = df2$col, 
		point_antialias = TRUE) # draw ellipsoids
	text3d(df1$x, df1$y, df1$z, texts = df1$sym, color = df1$col) # draw original points

	if (!is.null(truth)) { # X out errors in classification
		ans <- mclust::classError(mod$classification, truth)
		wh <- data[ans$misclassified,]
		if (length(wh) == 0) warning("No points were misclassified, damn you're good!")
		if (length(wh) > 0) text3d(wh, texts = "X", color = "black", cex = 1.5)
		}

	invisible(mod)
	}

plotScores <-
function(spectra, pca, title = "no title provided",
	pcs = c(1,2), ellipse = "none", tol = "none", ...) {

# Function to plot PCA scores
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Aug 2009
# Generates 2D plots of scores specified by argument pcs
# Part of this depends on a modified cor.plot {mvoutlier}
# which is called plotScoresCor

	if (length(pcs) != 2) stop("You must choose exactly two PC's to plot")
	if (missing(spectra)) stop("No spectral data set provided")
	if (missing(pca)) stop("No pca results provided")
	if (!("princomp" %in% class(pca) || "prcomp" %in% class(pca))) stop("Your pca results look corrupt!")
	chkSpectra(spectra)
	
	# Break the data into groups, clean up & give some warnings.
	# There must be at least 3 data points per level to make a classic ellipse.
	# More to make a robust ellipse, as at least one point may be dropped.

	lvls = levels(spectra$groups)
	count = length(lvls)
	for (n in 1:count) {
		ndp <- length(which(spectra$groups == lvls[n])) # How many members of each group are there?
		if (identical(ndp, 1)) warning("Group ", which(spectra$groups == lvls[n]), "has only 1 member.")
		if (identical(ndp, 2)) warning("Group ", which(spectra$groups == lvls[n]), "has only 2 members.")
		if (identical(ndp, 3)) warning("Group ", which(spectra$groups == lvls[n]), "has only 3 members.")
		}
	
	df <- data.frame(pca$x[,pcs], group = spectra$groups)
	groups <- dlply(df, "group", subset, select = c(1,2))

	col.lvls <- c() # set up the color scheme for ellipses, later
	for (n in 1:count) {
		chk <- match(lvls[n], spectra$groups) # get 1st instance
		col.lvls[n] <- spectra$colors[chk]
		}
		
### First case: plot everything by group (most general, other cases are subsets of this)

	if (ellipse == "both") {
	
	# First, get overall plot limits.
	# Keep in mind the ellipses may be quite flattened and hence large.
	# At the same time, the ellipses might be quite round and
	# the scores well outside them, if there is an outlier.
	# Must check all cases!
	
	ell <- llply(groups, plotScoresCor) # these are the ellipses we'll need later

	x.scores <- range(llply(groups, subset, select = 1))
	y.scores <- range(llply(groups, subset, select = 2)) 
	x.ell <- range(llply(ell, function(x) {range(x[1])}))
	y.ell <- range(llply(ell, function(x) {range(x[2])}))
	x.ell.r <- range(llply(ell, function(x) {range(x[4])}))
	y.ell.r <- range(llply(ell, function(x) {range(x[5])}))
	x.all <- range(x.scores, x.ell, x.ell.r)*c(1.0, 1.05) # expand slightly for labels
	y.all <- range(y.scores, y.ell, y.ell.r)
	y.all[2] <- y.all[2]*1.15 # leave room for annotations at top of plot

	# Plot the data points

	new.title <- paste(title, ": PCA Score Plot", sep = "") # prepare plot title

	plot(pca$x[,pcs], main = new.title, xlab = "", ylab = "",
	col = spectra$colors, xlim = x.all, ylim = y.all, pch = 20, ...)
	
	# Now plot both classic and robust ellipses, classic first

	cls.coords <- llply(ell, function(x) {x[1:2]})
	cls.coords <- llply(cls.coords, function(x) {do.call(cbind, x)})
	m_ply(cbind(x = cls.coords, col = col.lvls, lty = 3), lines, ...)

	# Now the robust ellipses
	
	rob.coords <- llply(ell, function(x) {x[4:5]})
	rob.coords <- llply(rob.coords, function(x) {do.call(cbind, x)})
	m_ply(cbind(x = rob.coords, col = col.lvls), lines, ...)

	# finish with the usual annotations
			
	legend("top", y = NULL, "classic ellipses by group", lty = 3, bty = "n", col = "gray", cex = 0.75)
	legend("top", y = NULL, "robust ellipses by group", lty = 1, bty = "n", col = "black", inset = c(0.0, 0.03), cex = 0.75)

	}

### Second case: plot only the points by group

	if (ellipse == "none") {
	
	x.scores <- range(llply(groups, subset, select = 1))
	y.scores <- range(llply(groups, subset, select = 2)) 
	x.all <- range(x.scores)*c(1.0, 1.05) # expand slightly for labels
	y.all <- range(y.scores)
	y.all[2] <- y.all[2]*1.15 # leave room for annotations at top of plot

	# Plot the data points

	new.title <- paste(title, ": PCA Score Plot", sep = "") # prepare plot title

	plot(pca$x[,pcs], main = new.title, xlab = "", ylab = "",
	col = spectra$colors, xlim = x.all, ylim = y.all, pch = 20, ...)
	
	}

### Third case: plot classical ellipses (only) by group

	if (ellipse == "cls") {
	
	ell <- llply(groups, plotScoresCor) # these are the ellipses we'll need later

	x.scores <- range(llply(groups, subset, select = 1))
	y.scores <- range(llply(groups, subset, select = 2)) 
	x.ell <- range(llply(ell, function(x) {range(x[1])}))
	y.ell <- range(llply(ell, function(x) {range(x[2])}))
	x.all <- range(x.scores, x.ell)*c(1.0, 1.05) # expand slightly for labels
	y.all <- range(y.scores, y.ell)
	y.all[2] <- y.all[2]*1.15 # leave room for annotations at top of plot

	# Plot the data points

	new.title <- paste(title, ": PCA Score Plot", sep = "") # prepare plot title

	plot(pca$x[,pcs], main = new.title, xlab = "", ylab = "",
	col = spectra$colors, xlim = x.all, ylim = y.all, pch = 20, ...)
	
	# Now plot classic ellipses

	cls.coords <- llply(ell, function(x) {x[1:2]})
	cls.coords <- llply(cls.coords, function(x) {do.call(cbind, x)})
	m_ply(cbind(x = cls.coords, col = col.lvls, lty = 3), lines, ...)

	# finish with the usual annotations
			
	legend("top", y = NULL, "classic ellipses by group", lty = 3, bty = "n", col = "gray", cex = 0.75)

	}

### Fourth case: plot robust ellipses (only) by group

	if (ellipse == "rob") {
	
	ell <- llply(groups, plotScoresCor) # these are the ellipses we'll need later

	x.scores <- range(llply(groups, subset, select = 1))
	y.scores <- range(llply(groups, subset, select = 2)) 
	x.ell.r <- range(llply(ell, function(x) {range(x[4])}))
	y.ell.r <- range(llply(ell, function(x) {range(x[5])}))
	x.all <- range(x.scores, x.ell.r)*c(1.0, 1.05) # expand slightly for labels
	y.all <- range(y.scores, y.ell.r)
	y.all[2] <- y.all[2]*1.15 # leave room for annotations at top of plot

	# Plot the data points

	new.title <- paste(title, ": PCA Score Plot", sep = "") # prepare plot title

	plot(pca$x[,pcs], main = new.title, xlab = "", ylab = "",
	col = spectra$colors, xlim = x.all, ylim = y.all, pch = 20, ...)
	
	# Now plot robust ellipses
	
	rob.coords <- llply(ell, function(x) {x[4:5]})
	rob.coords <- llply(rob.coords, function(x) {do.call(cbind, x)})
	m_ply(cbind(x = rob.coords, col = col.lvls), lines, ...)

	legend("top", y = NULL, "robust ellipses by group", lty = 1, bty = "n", col = "black", cex = 0.75)

	}

	# Decorations that apply to all cases
	
	plotScoresDecoration(spectra, pca, pcs, tol)
	
	leg.col <- c()
	for (z in 1:count) {
		i <- match(lvls[z], spectra$groups)
		leg.col[z] <- spectra$colors[i]
		}

	leg.txt <- c("Key", lvls)
	leg.col <- c("black", leg.col)
	legend("topright", leg.txt, text.col = leg.col, bty = "o", cex = 0.75)

}


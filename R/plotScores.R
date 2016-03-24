plotScores <-
function(spectra, pca,
	pcs = c(1,2), ellipse = "none", tol = "none",
	use.sym = FALSE, leg.loc = "topright", ...) {

# Function to plot PCA scores
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Aug 2009
# Generates 2D plots of scores specified by argument pcs
# Part of this depends on a modified cor.plot {mvoutlier}
# which is called plotScoresCor

	if (length(pcs) != 2) stop("You must choose exactly two PC's to plot")
	if (missing(spectra)) stop("No spectral data set provided")
	if (missing(pca)) stop("No pca results provided")
	if (!("princomp" %in% class(pca) || "prcomp" %in% class(pca))) stop("Your pca results object has the wrong class! Double check that the Spectra object is the 1st argument and the prcomp object is the 2nd argument.")
	chkSpectra(spectra)
	
	# There must be at least 3 data points per level to make a classic ellipse.
	# Possibly more to make a robust ellipse, as at least one point may be dropped.

	gr <- sumGroups(spectra)
	idx <- which(gr$no. > 3) # Index for which groups will get ellipses

	if (!ellipse == "none") {	
		for (n in 1:length(gr$group)) {
			if (gr$no.[n] == 1) warning("Group ", gr$group[n], " has only 1 member (no ellipse possible)")
			if (gr$no.[n] == 2) warning("Group ", gr$group[n], " has only 2 members (no ellipse possible)")
			if (gr$no.[n] == 3) warning("Group ", gr$group[n], " has only 3 members (ellipse not drawn)")
			}
		}
	df <- data.frame(pca$x[,pcs], group = spectra$groups)
	groups <- dlply(df, "group", subset, select = c(1,2))
		
### First case: plot everything by group (most general, other cases are subsets of this)

	if (ellipse == "both") {
	
	# First, get overall plot limits.
	# Keep in mind the ellipses may be quite flattened and hence large.
	# At the same time, the ellipses might be quite round and
	# the scores well outside them, if there is an outlier.
	# Must check all cases!
	
	ell <- llply(groups[idx], plotScoresCor) # these are the ellipses we'll need later

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

	if (!use.sym) plot(pca$x[,pcs], xlab = "", ylab = "",
	col = spectra$colors, xlim = x.all, ylim = y.all, pch = 20, ...)

	if (use.sym) plot(pca$x[,pcs], xlab = "", ylab = "",
	col = "black", xlim = x.all, ylim = y.all, pch = spectra$sym, ...)
	
	# Now plot both classic and robust ellipses, classic first

	cls.coords <- llply(ell, function(x) {x[1:2]})
	cls.coords <- llply(cls.coords, function(x) {do.call(cbind, x)})
	if (!use.sym) m_ply(cbind(x = cls.coords, col = gr$color[idx], lty = 3), lines, ...)
	if (use.sym) m_ply(cbind(x = cls.coords, col = "black", lty = 3), lines, ...)

	# Now the robust ellipses
	
	rob.coords <- llply(ell, function(x) {x[4:5]})
	rob.coords <- llply(rob.coords, function(x) {do.call(cbind, x)})
	if (!use.sym) m_ply(cbind(x = rob.coords, col = gr$color[idx]), lines, ...)
	if (use.sym) m_ply(cbind(x = rob.coords, col = "black"), lines, ...)

	# finish with the usual annotations
			
	legend("topleft", y = NULL, "classic ellipses by group", lty = 3, bty = "n", col = "gray", cex = 0.75, inset = c(0, 0.03))
	legend("topleft", y = NULL, "robust ellipses by group", lty = 1, bty = "n", col = "black", inset = c(0.0, 0.06), cex = 0.75)

	}

### Second case: plot only the points by group

	if (ellipse == "none") {
	
	x.scores <- range(llply(groups, subset, select = 1))
	y.scores <- range(llply(groups, subset, select = 2)) 
	x.all <- range(x.scores)*c(1.0, 1.05) # expand slightly for labels
	y.all <- range(y.scores)
	y.all[2] <- y.all[2]*1.15 # leave room for annotations at top of plot

	# Plot the data points

	if (!use.sym) plot(pca$x[,pcs], xlab = "", ylab = "",
	col = spectra$colors, xlim = x.all, ylim = y.all, pch = 20, ...)

	if (use.sym) plot(pca$x[,pcs], xlab = "", ylab = "",
	col = "black", xlim = x.all, ylim = y.all, pch = spectra$sym, ...)
	
	}

### Third case: plot classical ellipses (only) by group

	if (ellipse == "cls") {
	
	ell <- llply(groups[idx], plotScoresCor) # these are the ellipses we'll need later

	x.scores <- range(llply(groups, subset, select = 1))
	y.scores <- range(llply(groups, subset, select = 2)) 
	x.ell <- range(llply(ell, function(x) {range(x[1])}))
	y.ell <- range(llply(ell, function(x) {range(x[2])}))
	x.all <- range(x.scores, x.ell)*c(1.0, 1.05) # expand slightly for labels
	y.all <- range(y.scores, y.ell)
	y.all[2] <- y.all[2]*1.15 # leave room for annotations at top of plot

	# Plot the data points

	if (!use.sym) plot(pca$x[,pcs], xlab = "", ylab = "",
	col = spectra$colors, xlim = x.all, ylim = y.all, pch = 20, ...)

	if (use.sym) plot(pca$x[,pcs], xlab = "", ylab = "",
	col = "black", xlim = x.all, ylim = y.all, pch = spectra$sym, ...)
	
	# Now plot classic ellipses

	cls.coords <- llply(ell, function(x) {x[1:2]})
	cls.coords <- llply(cls.coords, function(x) {do.call(cbind, x)})
	if (!use.sym) m_ply(cbind(x = cls.coords, col = gr$color[idx], lty = 3), lines, ...)
	if (use.sym) m_ply(cbind(x = cls.coords, col = "black", lty = 3), lines, ...)

	# finish with the usual annotations
			
	legend("topleft", y = NULL, "classic ellipses by group", lty = 3, bty = "n", col = "gray", cex = 0.75, inset = c(0, 0.03))

	}

### Fourth case: plot robust ellipses (only) by group

	if (ellipse == "rob") {
	
	ell <- llply(groups[idx], plotScoresCor) # these are the ellipses we'll need later

	x.scores <- range(llply(groups, subset, select = 1))
	y.scores <- range(llply(groups, subset, select = 2)) 
	x.ell.r <- range(llply(ell, function(x) {range(x[4])}))
	y.ell.r <- range(llply(ell, function(x) {range(x[5])}))
	x.all <- range(x.scores, x.ell.r)*c(1.0, 1.05) # expand slightly for labels
	y.all <- range(y.scores, y.ell.r)
	y.all[2] <- y.all[2]*1.15 # leave room for annotations at top of plot

	# Plot the data points

	if (!use.sym) plot(pca$x[,pcs], xlab = "", ylab = "",
	col = spectra$colors, xlim = x.all, ylim = y.all, pch = 20, ...)

	if (use.sym) plot(pca$x[,pcs], xlab = "", ylab = "",
	col = "black", xlim = x.all, ylim = y.all, pch = spectra$sym, ...)
	
	# Now plot robust ellipses
	
	rob.coords <- llply(ell, function(x) {x[4:5]})
	rob.coords <- llply(rob.coords, function(x) {do.call(cbind, x)})
	if (!use.sym) m_ply(cbind(x = rob.coords, col = gr$color[idx]), lines, ...)
	if (use.sym) m_ply(cbind(x = rob.coords, col = "black"), lines, ...)

	legend("topleft", y = NULL, "robust ellipses by group", lty = 1, bty = "n", col = "black", cex = 0.75, inset = c(0, 0.03))

	}

	# Decorations that apply to all cases
	
	plotScoresDecoration(spectra, pca, pcs, tol)
	
	if (leg.loc == "none") return()
	leg.txt <- c("Key", gr$group)
	leg.col <- c("black", gr$color)
	if (use.sym) leg.col = "black"
	leg.pch <- NA
	if (use.sym) leg.pch <- c(NA, gr$sym)
	legend(leg.loc, leg.txt, text.col = leg.col, bty = "o", cex = 0.75, pch = leg.pch)

}


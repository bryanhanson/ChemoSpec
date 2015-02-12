
mclust3dSpectra <- function(spectra, pca, pcs = c(1:3),
	ellipse = TRUE, rob = FALSE, cl = 0.95, frac.pts.used = 0.8,
	truth = NULL, title = "no title provided", t.pos = NULL,
	lab.opts = FALSE, use.sym = FALSE, ...) {
	
# Wrapper to plot mclust results in 3D with confidence ellipses
# Bryan Hanson, DePauw University, Dec 2009
# Part of the ChemoSpec package

	mod <- mclust3D(pca$x[,pcs], ellipse = ellipse, rob = rob, cl = cl,
		frac.pts.used = frac.pts.used,
		truth = truth, title = title, t.pos = t.pos, lab.opts = lab.opts,
		use.sym = use.sym, ...)
	
	invisible(mod)
	}

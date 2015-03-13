
mclustSpectra <-
function(spectra, pca, pcs = c(1:3), dims = c(1,2),
	plot = c("BIC", "proj", "error"), use.sym = FALSE, ...) {

# Wrapper to mclust functions
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Dec 2009

	if (!requireNamespace("mclust", quietly = TRUE)) {
		stop("You need to install package mclust to use this function")
	}
	
	d <- pca$x[,pcs]
	mod <- mclust::Mclust(d, ...)
	note <- paste("Mclust optimal model: ", mod$modelName, "\n", sep = "")
	my.sym <- letters[1:length(unique(mod$classification))]

	if (plot == "BIC") {
		if (use.sym) plot(mod, d, what = "BIC", colors = "black")
		if (!use.sym) plot(mod, d, what = "BIC")
		sub <- paste(spectra$desc, pca$method, sep = "  ")
		title(sub = sub, cex.sub = 0.75)
		mtext(note, line = - 0.5)
		}
		
		
	if (plot == "proj") {
		if (!use.sym) {
			coordProjCS(d, dimens = dims, what = "classification",
				classification = mod$classification, parameters = mod$parameters,
				symbols = my.sym)
			}
		if (use.sym) {
			coordProjCS(d, dimens = dims, what = "classification",
				classification = mod$classification,
				parameters = mod$parameters, colors = "black")
			}
		sub <- paste(spectra$desc, pca$method, sep = "  ")
		title(sub = sub, cex.sub = 0.75)
		mtext(note, line = - 0.5)
		}

	if (plot == "errors") {
		coordProjCS(d, dimens = dims, what = "errors",
			classification = mod$classification, parameters = mod$parameters,
			truth = spectra$groups, symbols = my.sym)
		sub <- paste(spectra$desc, pca$method, sep = "  ")
		title(sub = sub, cex.sub = 0.75)
		mtext(note, line = - 0.5)		
		}
	invisible(mod)
	}

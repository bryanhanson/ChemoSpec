#'
#'
#' mclust Analysis of a Spectra Object PCA Results
#' 
#' This function is a wrapper for the \code{Mclust} function and associated
#' plotting functions.
#' 
#' 
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param pca An object of class \code{\link{prcomp}}.
#'
#' @param pcs An integer vector describing which PCs to use.
#'
#' @param dims A integer vector giving the PCA dimensions to use.
#'
#' @param plot A character string indicating what plot to make.  Options are
#' \code{c("BIC", "proj", "error")}; see \code{Mclust} for details.
#'
#' @param use.sym Logical; if true, the color scheme is changed to black and
#' symbols are used for plotting.
#'
#' @param \dots Other parameters to be passed downstream.
#'
#' @return The \code{Mclust} model is returned invisibly, and a plot is made.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso \code{\link[mclust]{Mclust}} for background on the method.
#'
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#'
#' @keywords multivariate cluster
#'
#' @examples
#' 
#' require("mclust")
#' data(SrE.IR)
#' class <- c_pcaSpectra(SrE.IR, choice = "autoscale")
#' mclustSpectra(SrE.IR, class, main = "Cuticle IR", plot = "BIC")
#' mclustSpectra(SrE.IR, class, main = "Cuticle IR", plot = "proj")
#' mclustSpectra(SrE.IR, class, main = "Cuticle IR", plot = "error",
#' 	 truth = metMUD1$groups)
#' 
#' @export mclustSpectra
#'
#' @importFrom graphics title mtext
# @importFrom mclust Mclust
#'
mclustSpectra <- function(spectra, pca, pcs = c(1:3), dims = c(1,2),
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

#'
#' Plot PCA Loadings for a Spectra Object
#' 
#' Creates a multi-panel plot of loadings along with a reference spectrum.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param pca An object of class \code{\link{prcomp}}, modified to include a
#' list element called \code{$method}, a character string describing the
#' pre-processing carried out and the type of PCA performed (it appears on the
#' plot).  This is automatically provided if \code{ChemoSpec} functions
#' \code{\link{c_pcaSpectra}} or \code{\link{r_pcaSpectra}} were used to create
#' \code{pca}.
#'
#' @param loads An integer vector giving the loadings to plot.  More than 3
#' loadings creates a useless plot using the default graphics window.
#'
#' @param ref An integer specifying the reference spectrum to plot, which
#' appears at the bottom of the plot.
#'
#' @param \dots Additional parameters to be passed to plotting functions.
#'
#' @return None.  Side effect is a plot.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso See \code{\link{plot2Loadings}} to plot two loadings against each
#' other, and \code{\link{sPlotSpectra}} for an alternative approach.
#' Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#'
#' @keywords multivariate hplot
#'
#' @examples
#' 
#' data(SrE.IR)
#' pca <- c_pcaSpectra(SrE.IR, choice = "noscale")
#' myt <- expression(bolditalic(Serenoa)~bolditalic(repens)~bold(IR~Spectra))
#' plotLoadings(SrE.IR, pca, main = myt,
#' 	 loads = 1:2, ref = 1)
#' 
#' @export plotLoadings
#'
#' @importFrom graphics plot
#' @importFrom stats relevel
#'
plotLoadings <- function(spectra, pca, loads = c(1), ref = 1, ...) {
	
	if (!requireNamespace("lattice", quietly = TRUE)) {
		stop("You need to install package lattice to use this function")
	}
	
	.chkArgs(mode = 12L)
	
	# Stack the requested data into a data frame for plotting
	
	names <- paste("PC", loads, "Loadings", sep = " ")
	names <- c("Reference Spectrum", names)
	x <- rep(spectra$freq, length(loads) + 1)
	
	z <- rep(names[1], length(spectra$freq))
	y <- spectra$data[ref,] # load in the reference spectrum
	
	for(n in 1:length(loads)) {
		y <- c(y, pca$rotation[,loads[n]]) # add in each loading
		z <- c(z, rep(names[n + 1], length(spectra$freq)))
		}

	z <- as.factor(z)
	z <- relevel(z, "Reference Spectrum")
	df <- data.frame(y, x, z) 
	
	# Do the plot
	# Note: no way exists to plot the x axis reversed for multiple panels

	p <- lattice::xyplot(y ~ x | z, data = df,
		xlab = spectra$unit[1], ylab = "",
		sub = list(label = pca$method,
			fontface = "plain"),
		# main = list(label = title,
			# fontface = "bold", cex = 1.5),
		layout = c(1, length(loads) + 1),
		strip.left = TRUE, strip = FALSE, col = "black",
		scales = list(x = "same", y = "free"),
		panel = function(..., type = "h") {
			if (lattice::panel.number() == 1) {
				lattice::panel.xyplot(..., type = "l")
				} else {
					lattice::panel.xyplot(..., type = type)
					}
			}, ...)
	
	plot(p)
	}


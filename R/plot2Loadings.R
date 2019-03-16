#'
#'
#' Plot PCA Loadings from a Spectra Object Against Each Other
#' 
#' Plots two PCA loadings specified by the user, and labels selected (extreme)
#' points.  Typically used to determine which variables (frequencies) are
#' co-varying, although in spectroscopy most peaks are represented by several
#' variables and hence there is a lot of co-varying going on.  Also useful to
#' determine which variables are contributing the most to the clustering on a
#' score plot.
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
#' @param loads A vector of two integers specifying which loading vectors to
#' plot.
#'
#' @param tol A number describing the fraction of points to be labeled.
#' \code{tol = 1.0} labels all the points; \code{tol = 0.05} labels the most
#' extreme 5 percent.
#'
#' @param \dots Other parameters to be passed to the plotting routines.
#'
#' @return None.  Side effect is a plot.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso See \code{\link{plotLoadings}} to plot one loading against the
#' original variable (frequency) axis.  See \code{\link{sPlotSpectra}} for
#' a different approach.  Additional documentation at
#' \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#'
#' @keywords multivariate hplot
#'
#' @examples
#' 
#' data(SrE.IR)
#' pca <- c_pcaSpectra(SrE.IR)
#' myt <- expression(bolditalic(Serenoa)~bolditalic(repens)~bold(IR~Spectra))
#' res <- plot2Loadings(SrE.IR, pca, main = myt,
#' 	 loads = c(1,2), tol = 0.001)
#' 
#' @export plot2Loadings
#'
#' @importFrom graphics plot abline legend
#' @importFrom ChemoSpecUtils .getVarExplained
#'
plot2Loadings <- function(spectra, pca, loads = c(1,2), tol = 0.05, ...) {
	
# Function to plot loadings against each other
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, June 2008

	.chkArgs(mode = 12L)
	if (length(loads) != 2) stop("You must choose exactly 2 loadings to plot.")
	chkSpectra(spectra)
	
	# pull the requested loadings

	loadings1 = pca$rotation[,loads[1]]
	loadings2 = pca$rotation[,loads[2]]
	
	variance <- .getVarExplained(pca)
	txt1 <- paste("PC", loads[1], " (", format(variance[loads[1]], digits=2), "%", ") loadings", sep = "")
	txt2 <- paste("PC", loads[2], " (", format(variance[loads[2]], digits=2), "%", ") loadings", sep = "")
	
	xrange <- range(loadings1)*c(1.0, 1.05) # makes room for labels
	yrange <- range(loadings2)*c(1.0, 1.05)

	plot(loadings1, loadings2, xlab = txt1, ylab = txt2, pch = 20, xlim = xrange, ylim = yrange, ...)
	abline(v = 0.0, col = "red")
	abline(h = 0.0, col = "red")
	legend("bottomleft", y = NULL, pca$method, bty = "n", cex = 0.75)

	# Next, if requested, we will label the extreme points on both dimensions
	
	if (is.numeric(tol)) .labelExtremes(pca$rotation[,loads], spectra$freq, tol)
	
	res <- data.frame(freq = spectra$freq, load1 = loadings1, load2 = loadings2)
	return(res)
	}


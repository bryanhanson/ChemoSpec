#'
#' s-Plot of Spectra Data (Post PCA)
#' 
#' Produces a scatter plot of the correlation of the
#' variables against their covariance for a chosen principal component.  It
#' allows visual identification of variables driving the separation and thus is
#' a useful adjunct to traditional loading plots.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param pca The result of a pca calculation on \code{\link{Spectra}} (i.e.
#' the output from \code{\link{c_pcaSpectra}} or \code{\link{r_pcaSpectra}}).
#'
#' @param pc An integer specifying the desired pc plot.
#'
#' @param tol A number describing the fraction of points to be labeled.
#' \code{tol = 1.0} labels all the points; \code{tol = 0.05} labels the most
#' extreme 5 percent.
#'
#' @param \dots Additional parameters to be passed to plotting functions.
#'
#' @return A data frame containing the frequency, covariance and correlation of
#' the selected pc for the \code{\link{Spectra}} object.  A plot of the
#' correlation vs. covariance is created.
#'
#' @author Matthew J. Keinsley and Bryan A. Hanson, DePauw University.
#'
#' @references Wiklund, Johansson, Sjostrom, Mellerowicz, Edlund, Shockcor,
#' Gottfries, Moritz, and Trygg. "Visualization of GC/TOF-MS-Based
#' Metabololomics Data for Identification of Biochemically Interesting
#' Compounds Usings OPLS Class Models" Analytical Chemistry Vol.80 no.1 pgs.
#' 115-122 (2008).
#' 
#' @seealso Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @keywords hplot
#'
#' @examples
#' 
#' data(SrE.IR)
#' IR.pca <- c_pcaSpectra(SrE.IR)
#' myt <- expression(bolditalic(Serenoa)~bolditalic(repens)~bold(IR~Spectra))
#' splot <- sPlotSpectra(spectra = SrE.IR, pca = IR.pca, pc = 1, tol = 0.001,
#'   main = myt)
#' 
#' @export sPlotSpectra
#'
#' @importFrom graphics plot abline legend
#'
sPlotSpectra <- function(spectra, pca, pc = 1, tol = 0.05, ...) {

##  Code to produce s-plots from Spectra objects
##  as in Wiklund.  Part of ChemoSpec
##  Matthew J. Keinsley
##  DePauw University, July 2011

	msg <- "This function cannot be used with data from sparse pca"
	if ("converted_from_arrayspc" %in% class(pca)) stop(msg)
	.chkArgs(mode = 12L)
	chkSpectra(spectra)
	if (length(pc) != 1) stop("You must choose exactly 1 pc to plot.")

	centspec <- scale(spectra$data, scale = FALSE)

	cv <- sdv <- c()
	
# Loop over each variable

	for (i in 1:ncol(centspec)) {
		tmp <- (pca$x[,pc] %*% centspec[,i])
		cv <- c(cv, tmp)
		dv <- sd(as.vector(centspec[,i])) # sd(matrix) deprecated for 2.14
		sdv <- c(sdv, dv)
	} 

	cv <- cv/(nrow(centspec)-1)
  	crr <- cv/(sdv*pca$sdev[pc])
	ans <- data.frame(freq = spectra$freq, cov = cv, corr = crr)

	plot(cv, crr, xlab = "covariance", ylab = "correlation",
		pch = 20, ...)
	abline(v = 0.0, col = "red")
	abline(h = 0.0, col = "red")
	legend("bottomright", y = NULL, pca$method, bty = "n", cex = 0.75)

	# Next, if requested, we will label the extreme points on both dimensions
	
	if (is.numeric(tol)) .labelExtremes(ans[,2:3], spectra$freq, tol)	
 
 	ans
	}



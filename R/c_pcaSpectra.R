#' Classical PCA of Spectra Objects
#' 
#' A wrapper which carries out classical PCA analysis on a
#' \code{\link{Spectra}} object.  The user can select various options for
#' scaling.  There is no normalization by rows - do this manually using
#' \code{\link{normSpectra}}.  There is an option to control centering, but
#' this is mainly for compatibility with the \code{\link{aov_pcaSpectra}}
#' series of functions.  Centering the data should always be done in PCA and it
#' is the default here.
#' 
#' The scale choice \code{autoscale} scales the columns by their standard
#' deviation.  \code{Pareto} scales by the square root of the standard
#' deviation.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#' 
#' @param choice A character string indicating the choice of scaling.  One of
#' \code{c("noscale"}, \code{"autoscale"}, \code{"Pareto")}.
#' 
#' @param cent Logical: whether or not to center the data.  Always center the
#' data unless you know it to be already centered.
#' 
#' @return An object of class \code{\link{prcomp}}, modified to include a list
#' element called \code{$method}, a character string describing the
#' pre-processing carried out and the type of PCA performed (it appears on
#' plots which you might make).
#' 
#' @author Bryan A. Hanson, DePauw University.
#' 
#' @seealso \code{\link{prcomp}} for the underlying function,
#' \code{\link{r_pcaSpectra}} for analogous robust PCA calculations.
#' 
#' For displaying the results, \code{\link{plotScree}},
#' \code{\link{plotScores}}, \code{\link{plotLoadings}},
#' \code{\link{plot2Loadings}}, \code{\link{sPlotSpectra}},
#' \code{\link{plotScores3D}}, \code{\link{plotScoresRGL}}.
#' 
#' @references K. Varmuza and P. Filzmoser \emph{Introduction to Multivariate
#' Statistical Analysis in Chemometrics}, CRC Press, 2009.
#' 
#' \url{https://github.com/bryanhanson/ChemoSpec}\cr
#' 
#' @keywords multivariate
#' 
#' @examples
#' 
#' data(metMUD1)
#' pca <- c_pcaSpectra(metMUD1)
#' plotScores(metMUD1, pca, main = "metMUD1 NMR Data",
#' 	 pcs = c(1,2), ellipse = "cls", tol = 0.05)
#' 
#' @export c_pcaSpectra
#' 
#' @importFrom stats sd prcomp
#' 
c_pcaSpectra <- function(spectra, choice = "noscale", cent = TRUE) {

# Function to carry out classical Principal Components Analysis
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Sept 2009
	
	if (missing(spectra)) stop("No spectral data set passed to PCA")
	
	choices <- c("noscale", "autoscale", "Pareto") # trap for invalid scaling method
	check <- choice %in% choices
	if (!check) stop("The choice of scaling parameter was invalid")
	chkSpectra(spectra)

	# Center & scale the data using the desired method.

	if (identical(choice, "noscale")) {centscaled <- scale(spectra$data, center = cent, scale = FALSE)}
	
	if (identical(choice, "autoscale")) {
		col.sd <- apply(spectra$data, 2, sd)
		centscaled <- scale(spectra$data, center = cent, scale = col.sd)}

	if (identical(choice, "Pareto")) {
		col.sd <- apply(spectra$data, 2, sd)
		centscaled <- scale(spectra$data, center = cent, scale = col.sd^0.5)}
	
	# Now the PCA!
	
	pca <- prcomp(centscaled, retx = TRUE, center = FALSE, scale. = FALSE)
	pca$method <- paste("centered/", choice, "/", "classical", sep = "")
	
	pca
				
	}


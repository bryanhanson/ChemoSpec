#' 
#' Plot aovPCAscores Loadings of a Spectra Object
#' 
#' Uses the results from \code{\link{aovPCAscores}} to plot the corresponding
#' loadings.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#' 
#' @param LM List of matrices created by \code{\link{aovPCAscores}}.
#' 
#' @param pca PCA output from \code{\link{aovPCAscores}}.
#' 
#' @param plot An integer specifying the desired plot. \code{names(LM)} will
#' show which matrix has which data in it.
#' 
#' @param loads An integer vector giving the loadings to plot.  More than 3
#' loadings creates a useless plot using the default graphics window.
#' 
#' @param ref An integer specifying the reference spectrum to plot, which
#' appears at the bottom of the plot.
#' 
#' @param \dots Additional parameters to be passed to plotting functions.
#' 
#' @return None. Side effect is a plot.
#' 
#' @author Matthew J. Keinsley and Bryan A. Hanson, DePauw University.
#' 
#' @seealso An example using this function can be seen in
#' \code{\link{aov_pcaSpectra}}.  See also \code{\link{plotLoadings}}.
#' Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#' 
#' @references Pinto, Bosc, Nocairi, Barros, and Rutledge. "Using ANOVA-PCA for
#' Discriminant Analysis: ..." Analytica Chimica Acta 629.1-2 (2008): 47-55.
#' 
#' Harrington, Vieira, Espinoza, Nien, Romero, and Yergey. "Analysis of
#' Variance--Principal Component Analysis: ..." Analytica Chimica Acta 544.1-2
#' (2005): 118-27.
#' 
#' @keywords multivariate htest
#' 
#' @export aovPCAloadings
#' 
aovPCAloadings <-function(spectra, LM, pca, plot = 1, loads = 1, ref = 1, ...) {

#  Function to plot Loadings of ANOVA-PCA per Harrington
#  Bryan Hanson and Matt Keinsley
#  DePauw University, June 2011

#  LM is the output from aov_pcaSpectra (a list of matrices)

	.chkArgs(mode = 11L)
	chkSpectra(spectra)
	
	if (plot > length(LM)){
		stop("Error, plot does not exist. Please choose a different plot!")}


##  Creation of titles for each graph depending on the number of factors and which graph was specified

	if (length(LM) == 3) {
		if (plot == 1) title = names(LM)[1]
		if (plot == 2) title = names(LM)[2]
		if (plot == 3) title = names(LM)[3]
		}
		
	if (length(LM) == 5) {
		if (plot == 1) title = names(LM)[1]
		if (plot == 2) title = names(LM)[2]
		if (plot == 3) title = names(LM)[3]
		if (plot == 4) title = names(LM)[4]
		if (plot == 5) title = names(LM)[5]
		}
		
	if (length(LM) == 8) {
		if (plot == 1) title = names(LM)[1]
		if (plot == 2) title = names(LM)[2]
		if (plot == 3) title = names(LM)[3]
		if (plot == 4) title = names(LM)[4]
		if (plot == 5) title = names(LM)[5]
		if (plot == 6) title = names(LM)[6]
		if (plot == 7) title = names(LM)[7]
		if (plot == 8) title = names(LM)[8]
		}

	
	plotLoadings(spectra = spectra, pca = pca, title = title, loads = loads, ref = ref, ...)	
	}

#'
#'
#' Conduct MANOVA using PCA Scores and Factors in a Spectra Object
#' 
#' This function provides a convenient interface for carrying out manova using
#' the scores from PCA and the factors (groups) stored in a
#' \code{\link{Spectra}} object.  The function will do anova as well, if you
#' only provide one vector of scores, though this is probably of limited use.
#' A \code{\link{Spectra}} object contains group information stored in its
#' \code{spectra$groups} element, but you can also use
#' \code{\link{splitSpectraGroups}} to generate additional groups/factors that
#' might be more useful than the original.
#' 
#' This function is an extraordinarily thin wrapper which helps the user to
#' avoid writing a very tedious \code{formula} specification.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#' 
#' @param pca An object of class \code{\link{prcomp}}.
#' 
#' @param pcs An integer vector giving the PCA scores to use as the response in
#' the manova analysis.
#' 
#' @param fac A character vector giving the factors to be used in the manova.
#' They will be searched for within the \code{\link{Spectra}} object.
#' 
#' @param \dots Additional arguments to be passed downstream, in this case to
#' \code{aov}.  Untested.
#' 
#' @return The results of the analysis print to the console unless assigned.
#' If assigned, the object class is one of several described in
#' \code{\link{aov}} depending upon the data passed to it.
#' 
#' @author Bryan A. Hanson, DePauw University.
#' 
#' @seealso \code{\link{splitSpectraGroups}} which can be used to create
#' additional factor elements in the \code{\link{Spectra}} object, which can then be
#' used with this function.
#' 
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#' 
#' @keywords multivariate htest
#' 
#' @examples
#' 
#' data(metMUD2)
#'
#' # Original factor encoding:
#' levels(metMUD2$groups)
#'
#' # Split those original levels into 2 new ones (re-code them)
#' new.grps <- list(geneBb = c("B", "b"), geneCc = c("C", "c"))
#' mM3 <- splitSpectraGroups(metMUD2, new.grps)
#'
#' # Now do the PCA and anova
#' pca <- c_pcaSpectra(mM3)
#' hypTestScores(mM3, pca, fac = c("geneBb", "geneCc"))
#' 
#' 
#' @export hypTestScores
#' 
#' @importFrom stats aov manova
#' 
hypTestScores <- function(spectra, pca, pcs = 1:3, fac = NULL, ...) {

# Function to carry out hypothesis test on PCA scores
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, August 2010

# This conducts a very simple hypothesis test, no contrasts or projections
# Fancier processing might be possible by using ... to pass along lm options

	if (missing(spectra)) stop("No spectral data set provided")
	if (missing(pca)) stop("No pca results provided")
	if (!("princomp" %in% class(pca) || "prcomp" %in% class(pca)))
		stop("Your pca results look corrupt!")
	if (is.null(fac)) stop("No factors specified")
	chkSpectra(spectra)
	
	scores <- pca$x[,pcs] # response vector
	
	# create formula
	
	form <- with(spectra, as.formula(paste("scores", "~", paste(fac, collapse = "*"))))

	
	# Do the hyp test; R knows if scores is multivariate or not,
	# but the summary format differs between aov and manova
	# 

	if (length(pcs) > 1) out <- manova(formula = form, ...)
	if (length(pcs) == 1) out <- aov(formula = form, ...)
	
	invisible(out)
	
	}

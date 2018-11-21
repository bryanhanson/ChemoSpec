#' Change the Color Scheme of a Spectra Object
#' 
#' This function permits you to change the color scheme of an existing
#' \code{\link{Spectra}} object.
#' 
#' A grepping process is used.  Depending upon the nature of the old color
#' names to be searched for, you might need to add some grep pattern matching
#' strings to the color name.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra}} whose color
#' scheme is to be changed.
#' 
#' @param old A character vector of the old color names; will be searched for
#' and replaced one-for-one by the character vector in \code{new}.
#' 
#' @param new A character vector of the new (replacement) color schemes.
#' 
#' @return An object of S3 class \code{\link{Spectra}} whose color scheme has
#' been changed.
#' 
#' @author Bryan A. Hanson, DePauw University.
#' 
#' @seealso For a discussion of general issues of color, see
#' \code{\link{colorSymbol}}. Additional documentation at
#' \url{https://bryanhanson.github.io/ChemoSpec/}
#' 
#' @keywords utilities color
#' 
#' @examples
#' 
#' data(metMUD1)
#' sumSpectra(metMUD1)
#' newSpec <- conColScheme(metMUD1, new = c("pink", "violet"))
#' sumSpectra(newSpec)
#' 
#' @export conColScheme
#' 
conColScheme <- function(spectra, old = levels(as.factor(spectra$colors)), new = NULL){
	
	.chkArgs(mode = 11L)
	chkSpectra(spectra)
	if (!length(old) == length(new)) stop("Old and new color schemes not of same length")
	new.spec <- spectra
	
	for (n in 1:length(old)) {
		which <- grep(old[n], spectra$colors)
		new.spec$colors[which] <- new[n]
		}

	chkSpectra(new.spec)
	new.spec
	}

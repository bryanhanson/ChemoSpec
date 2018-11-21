#'
#' Seriated Heat Map for a Spectra Object
#' 
#' Creates a heat map with marginal dendrograms using seriation procedures.
#' Very briefly, the samples that are most like each other occur in one corner,
#' and the frequencies that are most informative with respect to the samples
#' are in that corner as well. This is achieved by using heirchical cluster
#' analysis and then re-ordering the clusters in a coordinated way across each
#' dimension.  See the vignette for package \pkg{seriation}.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#' 
#' @param \dots Additional arguments to be passed downstream.  A great deal of
#' control is available - check \code{\link[seriation]{hmap}} for details.  Most of the control
#' actually derives from the \code{heatmap2} function in package \pkg{gplots}.
#' 
#' @return A list composed of two data frames. One is the frequencies and their
#' rankings, the other is samples and their rankings.  Side effect is a plot.
#' 
#' @author Bryan A. Hanson, DePauw University.
#' 
#' @seealso \code{\link[seriation]{hmap}} which will get you to the package
#' (there is no package index page); the vignette is a good place to begin
#' (\code{browseVignettes("seriation")}).  Additional documentation at
#' \url{https://bryanhanson.github.io/ChemoSpec/}
#' 
#' @keywords multivariate
#' 
#' @examples
#' 
#' data(SrE.IR)
#' # Let's look just at the carbonyl region
#' IR <- removeFreq(SrE.IR, rem.freq = SrE.IR$freq > 1850 | SrE.IR$freq < 1650)
#' res <- hmapSpectra(IR, col = heat.colors(5), labCol = FALSE)
#' 
#' @export hmapSpectra
#' 
# @importFrom seriation hmap
#' 
hmapSpectra <- function(spectra, ...) {
	
	.chkArgs(mode = 11L)
	chkSpectra(spectra)

	if (!requireNamespace("seriation", quietly = TRUE)) {
		stop("You need to install package seriation to use this function")
		}
		
	x.lab <- paste(spectra$unit[1], ", reordered", sep = "")
	
	res <- seriation::hmap(spectra$data, labRow = spectra$names,
		xlab = x.lab, ylab = "", margins = c(2, 6),
		key.title = "", ...)
		
	DF1 <- data.frame(freq = spectra$freq, rank = res$colInd)
	DF2 <- data.frame(sample = spectra$names, rank = res$rowInd)
	return(list(Freq = DF1, Sample = DF2))
	}
	

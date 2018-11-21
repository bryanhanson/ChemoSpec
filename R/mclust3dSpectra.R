#' 
#' mclust Analysis of a Spectra Object in 3D
#' 
#' This function conducts an mclust analysis of the PCA results of a
#' \code{\link{Spectra}} object and displays the results in 3D.  Classical or
#' robust confidence ellipses can be added if desired. Improperly classified
#' data points can be marked.  rgl graphics are employed.
#' 
#' If you intend to make a hard copy of your plot, use \code{lab.opts = TRUE}
#' until you have found a good view of your data.  Then note corners of the
#' cube where the title won't interfere with viewing the data, and use this for
#' \code{t.pos}, and add \code{title}.  Adjust as necessary, then turn off
#' label display using \code{lab.opts = FALSE}.  Back at the console, use
#' \code{> rgl.snapshot("file_name.png")} to create the hardcopy.
#' 
#' Note that the confidence ellipses computed here are generated independently
#' of the \code{Mclust} results - they do not correspond to the ellipses seen
#' in 2D plots from \code{Mclust}.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#' 
#' @param pca An object of class \code{\link{prcomp}}.
#' 
#' @param pcs An integer vector describing which PCs to use.
#' 
#' @param ellipse Logical indicating if confidence ellipses should be drawn.
#'
#' @param rob Logical; if \code{ellipse = TRUE}, indicates that robust
#' confidence ellipses should be drawn.  If \code{FALSE}, classical confidence
#' ellipses are drawn.
#' 
#' @param cl A number indicating the confidence interval for the ellipse.
#' 
#' @param frac.pts.used If \code{ellipse = TRUE} and \code{rob = TRUE}, a
#' number indicating the fraction of the data points to be considered "good"
#' and thus used to compute the robust confidence ellipse.
#' 
#' @param truth A character vector indicating the known group membership for
#' reach row of the PC scores.  Generally this would be \code{spectra$groups}.
#' #' @param title A character string for the plot title.
#' 
#' @param t.pos A character selection from \code{LETTERS[1:8]} ( = A through H)
#' indicating the desired location for the title.
#' 
#' @param title A character string giving the title.
#' 
#' @param lab.opts A logical indicating whether or not to display the locations
#' where the title and legend can be placed.  These locations are the corners
#' of a cube surrounding the data.
#' 
#' @param use.sym Logical; if true, the color scheme is changed to black and
#' symbols are used for plotting.
#' 
#' @param \dots Other parameters to be passed downstream.
#' 
#' @return The mclust model is returned invisibly, and a plot is produced.
#' 
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso \code{\link[mclust]{Mclust}} for background on the method.
#' Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#'
#' @keywords multivariate cluster
#' @examples
#' 
#' \dontrun{
#' require(mclust)
#' data(metMUD1)
#' class <- c_pcaSpectra(metMUD1)
#' mclust3dSpectra(metMUD1, class, title = "mclust3dSpectra demo",
#' 	 lab.opts = FALSE, t.pos = "A")
#' }
#' 
#' @export mclust3dSpectra
#'
mclust3dSpectra <- function(spectra, pca, pcs = c(1:3),
	ellipse = TRUE, rob = FALSE, cl = 0.95, frac.pts.used = 0.8,
	truth = NULL, title = "no title provided", t.pos = NULL,
	lab.opts = FALSE, use.sym = FALSE, ...) {
	
	.chkArgs(mode = 12L)
	
	mod <- .mclust3D(pca$x[,pcs], ellipse = ellipse, rob = rob, cl = cl,
		frac.pts.used = frac.pts.used,
		truth = truth, title = title, t.pos = t.pos, lab.opts = lab.opts,
		use.sym = use.sym, ...)
	
	invisible(mod)
	}

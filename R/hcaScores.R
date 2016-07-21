#'
#'
#' HCA on PCA scores from a Spectra Object
#' 
#' A wrapper which performs HCA on the scores from a PCA of a
#' \code{\link{Spectra}} object, color-coding the results as specified in the
#' object.  Many methods for computing the clusters and distances are
#' available.
#' 
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
#' @param scores A vector of integers specifying which scores to use for the
#' HCA.
#' 
#' @param c.method A character string describing the clustering method; must be
#' acceptable to \code{\link{hclust}}.
#' 
#' @param d.method A character string describing the distance calculation
#' method; must be acceptable as a method in \code{\link{rowDist}}.
#' 
#' @param use.sym A logical; if true, use no color and use lower-case letters
#' to indicate group membership.
#' 
#' @param leg.loc Character; if \code{"none"} no legend will be drawn.
#' Otherwise, any string acceptable to \code{\link{legend}}.
#' 
#' @param \dots Additional parameters to be passed to the plotting functions.
#' 
#' @return A list, containing an object of class \code{\link{hclust}} and an
#' object of class \code{\link{dendrogram}}.  The side effect is a plot.
#' 
#' @author Bryan A. Hanson, DePauw University.
#' 
#' @seealso \code{\link{hclust}} for the underlying function. See
#' \code{\link{hcaSpectra}} for HCA of the entire data set stored in the
#' \code{\link{Spectra}} object. \code{\link{plotHCA}} for the function that
#' actually does the plotting.
#' 
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#' 
#' @keywords multivariate cluster
#' 
#' @examples
#' 
#' data(SrE.IR)
#' pca <- c_pcaSpectra(SrE.IR, choice = "noscale")
#' myt <- expression(bolditalic(Serenoa)~bolditalic(repens)~bold(IR~Spectra))
#' res <- hcaScores(SrE.IR,  pca, scores = c(1:5), main = myt)
#' 
#' @export hcaScores
#' 
#' @importFrom stats hclust 
#' 
hcaScores <- function(spectra, pca, scores = c(1:5),
	c.method = "complete", d.method = "euclidean",
	use.sym = FALSE, leg.loc = "topright",  ...) {

# Function to carry out HCA on PCA scores
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Aug 2009

	
	if (missing(spectra)) stop("No spectral data set provided")
	if (missing(pca)) stop("No pca results provided")
	if (!("princomp" %in% class(pca) || "prcomp" %in% class(pca))) stop("Your pca results look corrupt!")
	chkSpectra(spectra)

	if (use.sym) spectra$names <- paste(spectra$alt.sym, spectra$names, sep = " ")
	distance <- rowDist(as.data.frame(pca$x[,scores], row.names = spectra$names), method = d.method)

	sub.title <- paste("clustering method: ", c.method, "      distance method: ", d.method, sep = "")

	# res <- plotHCA(spectra = spectra, distance = distance, sub.title = sub.title,
		# method = c.method, use.sym = use.sym, ...)
	# return(res)
	hclst <- hclust(distance, method = c.method)

	d <- plotHCA(spectra = spectra, hclst = hclst, sub.title = sub.title,
		use.sym = use.sym, leg.loc = leg.loc, ...)
	L = list(hclst = hclst, dend = d)
	return(L)
	}


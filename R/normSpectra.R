#'
#' Normalize a Spectra Object
#' 
#' This function carries out normalization of the spectra in a
#' \code{\link{Spectra}} object.  There are currently four options:
#' \itemize{
#'   \item \code{"PQN"} carries out "Probabalistic Quotient Normalization" as described
#'     in the reference.  This is probably the best option for many data sets.
#'   \item \code{"TotInt"} normalizes by total intensity.  In this
#'     case, the y-data of a \code{\link{Spectra}} object is normalized by dividing
#'     each y-value by the sum of the y-values in a given spectrum.  Thus each
#'     spectrum sums to 1.  This method assumes that the total concentration of
#'     all substances giving peaks does not vary across samples which may not be true.
#'   \item \code{"Range"} allows one to do something similar to \code{"TotInt"} but rather than using the
#'     sum of the entire spectrum as the denominator, only the sum of the given
#'     range is used.  This would be appropriate if there was an internal standard
#'     in the spectrum which was free of interferance, and one wanted to normalize
#'     relative to it.
#'   \item \code{"zero2one"} scales each spectrum separately to a [0 \ldots{} 1] scale.
#'     This is sometimes useful for visual comparison of chromatograms but is
#'     inappropriate for spectral data sets.
#' }
#' 
#' @param spectra An object of S3 class \code{\link{Spectra}} to be normalized.
#'
#' @param method One of \code{c("PQN", "TotInt", "Range", "zero2one")} giving
#' the method for normalization.
#'
#' @param RangeExpress A vector of
#' logicals (must be of \code{length(Spectra$freq)}).  This vector should be \code{TRUE} for
#' the frequency range you want to serve as the basis for norming, and \code{FALSE} otherwise.
#' The entire spectrum will be divided by the sum of the \code{TRUE} range.  See the examples.
#'
#' @return An object of S3 class \code{\link{Spectra}}.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @references Probabalistic Quotient Normalization is reported in F. Dieterle
#' et. al. Analytical Chemistry vol. 78 pages 4281-4290 (2006).  The exact same
#' mathematics are called "median fold change normalization" by Nicholson's
#' group, reported in K. A. Veselkov et. al. Analytical Chemistry vol. 83 pages
#' 5864-5872 (2011).
#' 
#' @seealso Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @keywords utilities manip
#'
#' @examples
#' 
#' data(SrE.IR)
#'
#' # Default PQN normalization
#' res1 <- normSpectra(SrE.IR)
#' plotSpectra(res1) # compare to plotSpectra(SrE.IR)
#'
#' # Norm over carbonyl region
#' RE <- SrE.IR$freq > 1650 & SrE.IR$freq < 1800
#' res2 <- normSpectra(SrE.IR, method = "Range", RangeExpress = RE)
#' plotSpectra(res2) # compare to plotSpectra(SrE.IR)
#'
#' # Check numerically
#' rowSums(res2$data[,RE]) # compare to rowSums(SrE.IR$data[,RE])
#'  
#' @export normSpectra
#'
#' @importFrom stats median
#'
normSpectra <- function(spectra, method = "PQN", RangeExpress = NULL) {
	
# Function to Normalize the data in a Spectra object
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Nov 2009

	.chkArgs(mode = 11L)
	chkSpectra(spectra)

# normalize using the probablistic quotient normalization (PQN)

	if (method == "PQN") {
		
		# Do a standard TotInt normalization
		S <- normSpectra(spectra, method = "TotInt")$data
		if (any(S < 0)) S <- S - min(S)
		
		# Compute the median spectrum for reference
		M <- apply(S, 2, median)

		# Divide each normed spectrum by the reference column medians (the ref spectrum)
		F <- S
		for (i in 1:nrow(F)) F[i,] <- F[i,]/M
		
		# Get the row medians (per spectrum median) of the ratioed spectra
		# These are the apparent 'fold' dilution factors
		# for each spectrum/sample
		F <- apply(F, 1, median)
		
		# Divide each row of the original data by it's median
		for (i in 1:nrow(S)) S[i,] <- S[i,]/F[i]
		
		spectra$data <- S
		}

# normalize a row by the sum of its entries:

	if (method == "TotInt") {
		for (n in 1:length(spectra$names)) {
			S <- sum(as.numeric(spectra$data[n,]))
			spectra$data[n,] <- spectra$data[n,]/S
			}
		}

# normalize by a range of specified values:

	if (method == "Range") {
		if (is.null(RangeExpress)) stop("No range expression given")
		rfi <- which(RangeExpress)
		for (n in 1:length(spectra$names)) {
			S <- sum(as.numeric(spectra$data[n,rfi]))
			spectra$data[n,] <- spectra$data[n,]/S
			}
		}

# normalize each spectrum to a [0...1] range:

	if (method == "zero2one") {
		for (i in 1:length(spectra$names)) {
			rMin <- min(spectra$data[i,])
			spectra$data[i,] <- spectra$data[i,] - rMin
			rMax <- max(spectra$data[i,])
			spectra$data[i,] <- spectra$data[i,]/rMax
			}
		}

	chkSpectra(spectra)
	spectra
	}

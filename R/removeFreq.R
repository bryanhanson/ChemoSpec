#'
#'
#' Remove Frequencies from a Spectra Object
#' 
#' This function removes specified frequencies from a \code{\link{Spectra}}
#' object.  For instance, one might want to remove regions lacking any useful
#' information (to reduce the data size), or remove regions with large
#' interfering peaks (e.g. the water peak in 1H NMR).
#' 
#' \code{rem.freq} can be any valid \code{R} statement that leads to a vector of
#' logicals.  In the examples below, the | and & operators seem backward in
#' a sense, but R evaluates them one at a time and combines the result to
#' give the required output.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra}} from which to
#' remove selected frequencies.
#'
#' @param rem.freq A valid R statement describing the frequencies to be
#' removed.  This must comply with \code{\link{Comparison}} and
#' \code{\link{Logic}}.  See the examples below for common usage.
#'
#' @return An object of S3 class \code{\link{Spectra}}.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#'
#' @keywords utilities manip
#'
#' @examples
#' 
#' data(SrE.IR)
#' sumSpectra(SrE.IR)
#'
#' # Remove frequencies from one end:
#' newIR <- removeFreq(SrE.IR, rem.freq = SrE.IR$freq > 3500)
#'
#' # Remove frequencies from both ends at once:
#' newIR <- removeFreq(SrE.IR, rem.freq = SrE.IR$freq > 3500
#'  | SrE.IR$freq < 800)
#'
#' # Remove frequencies from the middle:
#' newIR <- removeFreq(SrE.IR, rem.freq = SrE.IR$freq > 800
#'   & SrE.IR$freq < 1000)
#' 
#' # The logic of this last one is as follows.  Any values
#' # that are TRUE will be removed.
#' values <- 1:7
#' values > 2
#' values < 6
#' values > 2 & values < 6
#' 
#' # After any of these, inspect the results:
#' sumSpectra(newIR)
#' check4Gaps(newIR$freq, newIR$data[1,], plot = TRUE)
#' 
#' @export removeFreq
#'
removeFreq <- function(spectra, rem.freq) {

# Function to remove selected frequencies from a Spectra object
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Nov 2009
# Major revision, July 2010

	if (missing(spectra)) stop("No spectral data provided")
	if (missing(rem.freq)) stop("Nothing to remove")
	chkSpectra(spectra)
	
	# rem.freq must be a character string giving a valid logical statement of freq to be removed
	# generally should be a combination of ?Comparison and ?base::Logic concepts.	
	rfi <- which(rem.freq)
	spectra$data <- spectra$data[,-c(rfi), drop = FALSE]
	spectra$freq <- spectra$freq[-c(rfi)]
	chkSpectra(spectra)
		
	spectra
	}


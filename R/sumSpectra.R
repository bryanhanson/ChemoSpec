#'
#' Summarize a Spectra Object
#' 
#' Provides a summary of a \code{\link{Spectra}} object, essentially a more
#' spectroscopist-friendly version of \code{str()}.
#' 
#' Prior to summarizing, \code{\link{chkSpectra}} is run with confirm = FALSE.
#' If there are problems, warnings are issued to the console and the summary is
#' not done.  If \code{sumSpectra} thinks there is a gap between every data
#' point, add the argument \code{tol = xx} which will pass through to
#' \code{\link{check4Gaps}} and alleviate this problem (which has to do with
#' rounding when subtracting two adjacent frequency values).  The
#' \code{\link{Spectra}} object is checked to see if it contains data elements
#' beyond what is required.  If so, these extra elements are reported to the
#' console.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param ...  Arguments to be passed downstream.
#'
#' @return None.  Results printed at console, perhaps a plot as well.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#'
#' @keywords utilities
#'
#' @examples
#' 
#' data(metMUD1)
#' sumSpectra(metMUD1)
#' 
#' 
#' @export sumSpectra
#'
sumSpectra <- function(spectra, ...){
	
# Function to summarize objects of S3 class 'Spectra'
# Part of ChemoSpec package
# Bryan Hanson, DePauw Univ, Nov 2009
	
	chkSpectra(spectra) # verify it's legit
	
	# Try to determine a sensible value for tol if none provided via the ...
	
	args <- names(as.list(match.call()[-1]))

	if (!("tol" %in% args)) {
		# Improvements to the automatic calc of a suitable tol value suggested
		# by Dana Nadler in e-mails, early March 2017.  Any errors are mine however!		
		# fdiff includes normal data resolution & any larger gaps
		# hist(fdiff) should be dominated by data resolution
		# unique(fdiff) shows that even basic data resolution suffers from encoding differences
		fdiff <- diff(spectra$freq) 
		tol <- abs(median(fdiff)) * 1.2 # ensures value is a bit larger than nominal resolution
		h <- check4Gaps(spectra$freq, tol = tol)	
		}
	

	if ("tol" %in% args) h <- check4Gaps(spectra$freq, ...)	

	# Other summaries
	
	g <- sumGroups(spectra)
	#res <- abs(spectra$freq[2] - spectra$freq[1])
	res <- abs(median(diff(spectra$freq))) # consistent with method of determining tol above
	
	# Now print main summary to console
	
	cat("\n", spectra$desc, "\n\n")
	cat("\tThere are ", length(spectra$names), " spectra in this set.\n", sep = "")
	cat("\t", "The y-axis unit is ", spectra$unit[2], ".\n\n", sep = "")
	cat("\tThe frequency scale runs from ", spectra$freq[1], " to ", 
		spectra$freq[length(spectra$freq)], " ", spectra$unit[1], "\n", sep = "")
	cat("\tThere are ", length(spectra$freq), " frequency (x-axis) data points.\n", 
		sep = "")
	cat("\tThe frequency resolution is ", res, " ", spectra$unit[1], "/point.\n\n", sep = "")
	if (length(h) > 1) {
		cat("\tThis data set is not continuous along the frequency axis.\n")
		cat("\tHere are the data chunks:\n\n")
		print(h)
		}
	cat("\n")
	cat("\tThe spectra are divided into", length(levels(spectra$groups)), "groups:", "\n\n")
	print(g)
	
	# Check for extra data and report if found
	
	sn <- names(spectra)
	tn <- c("freq", "data", "names", "groups", "colors", "sym", "alt.sym", "unit", "desc")
	extra <- setdiff(sn, tn)
	if (length(extra) > 0) {
		msg <- paste("\n\tAdditional data was found:", extra, "\n", sep = " ")
		cat(msg)
		}
	
	cat("\n*** Note: this data is an S3 object of class 'Spectra'\n")
	}


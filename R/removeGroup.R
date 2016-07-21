#'
#'
#' Remove Groups or Samples from a Spectra Object
#' 
#' Removes specified groups or samples from a \code{\link{Spectra}} object.
#' 
#' Both functions will report if extra data elements are found.  These will
#' probably need to be edited manually.  The indices reported to the console
#' can be helpful in this regard.
#'
#' If \code{rem.sam} is a character vector, the sample
#' names are grepped for the corresponding values.  \code{rem.group}
#' also uses grep.  Remember that the
#' grepping process is greedy, i.e. grepping for "XY" find not only "XY" but
#' also "XYZ".
#'
#' Unused levels in \code{$groups} are dropped.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param rem.group A character vector giving the groups to be removed.
#'
#' @param rem.sam Either an integer vector specifying the samples to be
#' removed, or a character vector giving the sample names to be removed.
#' 
#' @return A modified object of S3 class \code{\link{Spectra}}.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso \code{\link{removeFreq}} to remove selected frequencies.
#'
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#'
#' @keywords utilities manip
#'
#' @examples
#' 
#' data(metMUD1)
#'
#' # removeGroup
#' sumSpectra(metMUD1)
#' trmt <- removeGroup(metMUD1, rem.group = "Cntrl")
#' sumSpectra(trmt)
#'
#' # removeSample
#' # Removes the 20th spectrum/sample:
#' new1 <- removeSample(metMUD1, rem.sam = 20)
#'
#' # Removes one spectrum/sample with this exact name:
#' new2 <- removeSample(metMUD1, rem.sam = "Sample_20")
#'
#' # Opps! Removes all samples due to greedy grep!
#' new3 <- removeSample(metMUD1, rem.sam = "Sample")
#' 
#' @export removeGroup removeSample
#'
#' @describeIn removeGroup Remove groups from a \code{Spectra} object
#'
removeGroup <- function(spectra, rem.group) {
	
# Function to Remove Selected Groups
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, May 2012

	if (missing(spectra)) stop("No spectral data provided")
	if (missing(rem.group)) stop("Nothing to remove")
	chkSpectra(spectra)
	
	# BE CAREFUL: greping can catch more than you think!
	
	k <- c()
	if (is.character(rem.group)) {
		for (n in 1:length(rem.group)) {
			more <- grep(rem.group[n], spectra$groups)
			k <- c(k, more)
			}
		rem.group <- k
		}

	if (max(rem.group) > length(spectra$groups)) stop("Groups to remove are out of range")

	spectra$data <- spectra$data[-rem.group,]
	spectra$names <- spectra$names[-rem.group]
	spectra$groups <- spectra$groups[-rem.group, drop = TRUE]
	spectra$colors <- spectra$colors[-rem.group]
	spectra$sym <- spectra$sym[-rem.group]
	spectra$alt.sym <- spectra$alt.sym[-rem.group]

	sn <- names(spectra)
	tn <- c("freq", "data", "names", "groups", "colors", "sym", "alt.sym", "unit", "desc")
	extra <- setdiff(sn, tn)
	if (length(extra) > 0) {
		msg <- paste("Additional data was found:", extra, "and not modified\n", sep = " ")
		message(msg)
		message("If these are per sample data, you may have to manually edit them")
		msg <- paste("The removal indices are:", rem.group, sep = " ")
		message(msg)
		
		}
	
	if (length(spectra$groups) == 0) warning("You have removed all your samples!")


	chkSpectra(spectra)
	spectra

	}


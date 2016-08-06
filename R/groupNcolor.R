#'
#'
#' Assign Group Membership and Colors for a Spectra Object
#' 
#' A utility function which looks for \code{gr.crit} in the file names of .csv
#' files and assigns group membership.  Also assigns a color, a symbol and an
#' alternate symbol to each group.  Warnings are given if there are file names
#' that don't match entries in \code{gr.crit} or there are entries in
#' \code{gr.crit} that don't match any file names.  An internal function, not
#' generally called by the user.
#' 
#' 
#' @param spectra An object of S3 class \code{\link{Spectra}}.  Until this
#' function acts on \code{spectra} it is not quite complete.
#' 
#' @param gr.crit As per \code{\link{files2SpectraObject}}.
#' 
#' @param gr.cols As per \code{\link{files2SpectraObject}}.
#' 
#' @return A \emph{complete} object of S3 class \code{\link{Spectra}}.  This
#' function is the last internal step in creating a \code{Spectra} object.
#' Until this function has done its job, an object of class
#' \code{\link{Spectra}} will not pass checks as the assembly is not complete
#' (see \code{\link{chkSpectra}}).
#' 
#' @author Bryan A. Hanson, DePauw University.
#' 
#' @seealso \code{\link{files2SpectraObject}} for details;
#' \code{\link{sumGroups}} to see the outcome.
#' 
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#' 
#' @keywords utilities color
#' 
#' @export groupNcolor
#'
# @importFrom RColorBrewer brewer.pal
#'
groupNcolor <- function(spectra, gr.crit = NULL, gr.cols = c("auto")) {

# Function to Process Group Assignments & Assign Colors
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Aug 2009
# Acts on all csv files in a directory.

	# use the group criteria (gr.crit) to classify the samples

	spectra$groups <- rep(NA, length(spectra$names)) # intialize
	
	# Change: rather than re-read the file names, which are already
	# incorporated into spectra$names, search spectra$names.
	# This makes it easier to semi-manually construct the full spectra object
	
	#files <- list.files(pattern = "\\.(csv|CSV)")
	#files.noext <- substr(basename(files), 1, nchar(basename(files)) - 4)
	
	# for (i in 1:length(gr.crit)) {
		# which <- grep(gr.crit[i], files.noext)
		# spectra$groups[which] <- gr.crit[i]
		# }

	for (i in 1:length(gr.crit)) {
		which <- grep(gr.crit[i], spectra$names)
		if (length(which) == 0) warning("There was no match for gr.crit value ", gr.crit[i], " among the file names")
		spectra$groups[which] <- gr.crit[i]
		}
	
	spectra$groups <- as.factor(spectra$groups)
	if (any(is.na(spectra$groups))) warning("At least one file name did not correspond any entry in gr.crit and its group is thus NA")
	
	# Assign each group a color for plotting later

	spectra$colors <- rep(NA, length(spectra$names)) # initialize
	
	if (identical(gr.cols[1], "auto")) {
		if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
			stop("You need to install package RColorBrewer or supply the colors yourself")
			}
		gr.cols <- RColorBrewer::brewer.pal(length(gr.crit), "Set1")
		for (i in 1:length(gr.crit)) {
			which <- grep(gr.crit[i], spectra$names)
			spectra$colors[which] <- gr.cols[i]
			}
		}
		
	if (!identical(gr.cols[1], "auto")) {
		for (i in 1:length(gr.crit)) {
			which <- grep(gr.crit[i], spectra$groups)
			spectra$colors[which] <- gr.cols[i]
			}
		}

	# Either way, associate symbols and alt.sym with each gr.crit
	sym.choice <- c(1, 2, 3, 15, 16, 17, 22, 8) # preferred symbols
	sym1 <- sym.choice[1:length(gr.crit)]
	sym2 <- letters[1:length(gr.crit)]
	if (length(gr.crit) > 8) {
		sym1 <- c(1:length(gr.crit))
		warning("Too many groups to use the preferred symbols!")
		}

	for (i in 1:length(gr.crit)) {
		which <- grep(gr.crit[i], spectra$groups)
		spectra$sym[which] <- sym1[i]
		spectra$alt.sym[which] <- sym2[i]
		}
	
	class(spectra) <- "Spectra"
	return(spectra) # spectra is now complete!
	}


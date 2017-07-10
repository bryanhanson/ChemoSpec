#'
#'
#' Assign Group Membership and Colors for a Spectra Object
#' 
#' A utility function which looks for \code{gr.crit} in the file names of .csv
#' files and assigns group membership (max 8 groups automatically).  Also assigns a color, a symbol and an
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

# Function to Process Group Assignments & Assign Colors & Symbols
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Aug 2009

# Revision March 2017 to better handle large numbers of groups, and provide more checks and warnings.
# A max of 8 groups can be handled automatically, assigning colors and symbols.  Beyond that, users
# are warned they will have to fix things manually.

	msg1 <- "At least one file name did not correspond any entry in gr.crit and its group is thus NA"
    msg2 <- "More groups than colors, colors will be recycled.\n  Redefine groups or specify colors manually."
    msg3 <- "Too many groups to use the preferred symbols; setting all symbols to 1\n  and alt.sym to 'a'. Assign symbols manually."
    
	# Use the group criteria (gr.crit) to classify the samples

	spectra$groups <- rep(NA_character_, length(spectra$names))
	
	for (i in 1:length(gr.crit)) {
		which <- grep(gr.crit[i], spectra$names)
		if (length(which) == 0) warning("There was no match for gr.crit value ", gr.crit[i], " among the file names.")
		spectra$groups[which] <- gr.crit[i]
		}
	
	spectra$groups <- as.factor(spectra$groups)
	if (any(is.na(spectra$groups))) warning(msg1)
	
	# Assign each group a color for plotting later

	spectra$colors <- rep(NA_character_, length(spectra$names))
	
	if (identical(gr.cols[1], "auto")) {
		if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
			stop("You need to install package RColorBrewer or supply the colors yourself")
			}

		if (length(gr.crit) > 8) warning(msg2)
		cscols <- RColorBrewer::brewer.pal(8, "Set1") # 9 colors in Set1, only using 8 so as to match symbol restrictions
		gr.cols <- cscols[1:length(gr.crit)]

		for (i in 1:length(gr.crit)) {
			which <- grep(gr.crit[i], spectra$names)
			spectra$colors[which] <- gr.cols[i]
			}
		}
		
	if (!identical(gr.cols[1], "auto")) {
		if (length(gr.cols) != length(gr.crit)) stop("Length of gr.cols and gr.crit did not match")
		for (i in 1:length(gr.crit)) {
			which <- grep(gr.crit[i], spectra$groups)
			spectra$colors[which] <- gr.cols[i]
			}
		}

	# Associate symbols and alt.sym with each gr.crit
	
	sym.choice <- c(0, 1, 2, 3, 15, 16, 17, 8) # preferred symbols (8 of them)
	
	if (length(gr.crit) > 8) {
		spectra$sym <- rep(1L, length(spectra$names)) 
		spectra$alt.sym <- rep("a", length(spectra$names))
		warning(msg3)
		}
		
	if (length(gr.crit) <= 8) {
		sym1 <- sym.choice[1:length(gr.crit)]
		sym2 <- letters[1:length(gr.crit)]
		for (i in 1:length(gr.crit)) {
			which <- grep(gr.crit[i], spectra$groups)
			spectra$sym[which] <- sym1[i]
			spectra$alt.sym[which] <- sym2[i]
			}
		}
		
	class(spectra) <- "Spectra"
	
	return(spectra) # spectra is now complete!
	}


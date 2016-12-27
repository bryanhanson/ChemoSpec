#'
#'
#' Import Data into a Spectra Object
#'
#' These functions import data into a \code{\link{Spectra}} object.  They use
#' \code{\link[utils]{read.table}} to read files so they are
#' very flexible in regard to file formatting.  \pkg{Be sure to see the \ldots
#' argument below for important details you need to provide.}
#' 
#' The matching of \code{gr.crit} against the sample file names is done one at
#' a time, in order.  This means that the entries in \code{gr.crit} must be
#' mutually exclusive.  For example, if you have files with names like
#' "Control_1" and "Sample_1" and use \code{gr.crit = c("Control", "Sample")}
#' groups will be assigned as you would expect.  But, if you have file names
#' like "Control_1_Shade" and "Sample_1_Sun" you can't use \code{gr.crit =
#' c("Control", "Sample", "Sun", "Shade")} because each criteria is grepped in
#' order, and the "Sun/Shade" phrases, being last, will form the basis for your
#' groups.  Because this is a grep process, you can get around this by using
#' regular expressions in your \code{gr.crit} argument to specify the desired
#' groups in a mutually exclusive manner.  In this second example, you could
#' use \code{gr.crit = c("Control(.*)Sun", "Control(.*)Shade", "Sample(.*)Sun",
#' "Sample(.*)Shade")} to have your groups assigned based upon both phrases in
#' the file names.
#' 
#' The linking of groups with colors is handled by \code{\link{groupNcolor}}.
#' 
#' Samples whose names are not matched using \code{gr.crit} are still
#' incorporated into the \code{\link{Spectra}} object, but they are not
#' assigned a group or color. Therefore they don't plot, but they do take up space in a
#' plot!  A warning is issued in these cases, since one wouldn't normally want
#' a spectrum to be orphaned this way.
#'
#' If \code{fileExt} contains any of \code{"dx"}, \code{"DX"}, \code{"jdx"} or
#' \code{"JDX"}, then the files will be processed by \code{\link[readJDX]{readJDX}}.
#' Consider setting \code{debug = TRUE} for this format, as there are many
#' options for JCAMP, and many are untested. See \code{\link[readJDX]{readJDX}} for
#' known limitations.
#' 
#' 
#' @param gr.crit Group Criteria.  A vector of character strings which will be
#' searched for among the file names in order to assign an individual
#' spectrum/sample to group membership. Warnings are issued if there are file
#' names that don't match entries in \code{gr.crit} or there are entries in
#' \code{gr.crit} that don't match any file names. See Details for some
#' nuances.
#' 
#' @param gr.cols Group Colors.  Either the word "auto", in which case colors
#' will be automatically assigned, or a vector of acceptable color names with
#' the same length as \code{gr.crit}. In the latter case, colors will be
#' assigned one for one, so the first element of \code{gr.crit} is assigned the
#' first element of \code{gr.col} and so forth.  See details below for some
#' other issues to consider.
#' 
#' @param freq.unit A character string giving the units of the x-axis
#' (frequency or wavelength).
#' 
#' @param int.unit A character string giving the units of the y-axis (some sort
#' of intensity).
#' 
#' @param descrip A character string describing the data set that will be
#' stored.  This string is used in some plots so it is recommended that its
#' length be less than about 40 characters.
#' 
#' @param fileExt A character string giving the extension of the files to be
#' processed. \code{regex} strings can be used.  For instance, the default
#' finds files with either \code{".csv"} or \code{".CSV"} as the extension.
#' Matching is done via a grep process, which is greedy.
#' 
#' @param out.file A file name.  The
#' completed object of S3 class \code{\link{Spectra}} will be written to this
#' file.
#' 
#' @param debug Logical. Applies to \code{files2SpectraObject} only.
#' Set to \code{TRUE} for troubleshooting when an error
#' is thrown during import.  In addition, values of 1-5 will work
#' when importing a JCAMP-DX file via \code{fileExt = ".jdx"} etc.  These
#' will be passed through to the \code{\link[readJDX]{readJDX}} function.
#' See there for much more info on importing JCAMP-DX files.
#' 
#' @param in.file Character.  Applies to \code{matrix2SpectraObject} only.
#' Input file name, including extension.
#'  
#' @param chk Logical. Applies to \code{matrix2SpectraObject} only.
#' Should the \code{Spectra} object be checked for
#' integrity?  If you are having trouble importing your data, set this to
#' \code{FALSE} and do \code{str(your object)} to troubleshoot.
#'
#' @param ...  Arguments to be passed to \code{\link[utils]{read.table}}.  \pkg{You
#' MUST supply values for \code{sep}, \code{dec} and \code{header} consistent
#' with your file structure, unless they are the same as the defaults for
#' \code{\link[utils]{read.table}}}.
#' 
#' @return A object of class \code{\link{Spectra}}.  An \emph{unnamed} object
#' of S3 class \code{\link{Spectra}} is also written to \code{out.file}.  To
#' read it back into the workspace, use \code{new.name <- loadObject(out.file)}
#' (\code{loadObject} is package \pkg{R.utils}).
#' 
#' @section files2SpectraObject:
#' \code{files2SpectraObject} acts on all files in the current working
#' directory with the specified \code{fileExt}.  The first column should
#' contain the frequency values and the second column the intensity values. The
#' files may have a header or not (supply \code{header = TRUE/FALSE} as
#' necessary).  The frequency column is assumed to be the same in all files.
#' 
#' @section matrix2SpectraObject:
#' This function takes a csv-like file, containing frequencies in the first
#' column, and samples in additional columns, and processes it into a
#' \code{\link{Spectra}} object.  The file MUST have a header row which includes
#' the sample names.  There need not be a header for the first (frequency)
#' column.
#'
#' @author Bryan A. Hanson, DePauw University.
#' 
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#' 
#' @keywords file
#' @keywords import
#' 
#' @export files2SpectraObject matrix2SpectraObject
#'
#' @describeIn files2SpectraObject Import data from separate csv files
#' 
#' @importFrom utils read.table
#' @importFrom tools file_path_sans_ext
#'
#' @examples
# Grab an included file and move it to a temporary directory
#' td <- tempdir()
#' ed <- system.file("extdata", package = "ChemoSpec")
#' tf <- "PCRF.jdx"
#' chk <- file.copy(from = file.path(ed, tf), to = file.path(td, tf),
#' 	overwrite = TRUE)
#' setwd(td)
# Now read in the file, and plot
#' spec <- files2SpectraObject(gr.crit = "PCRF", freq.unit = "ppm", int.unit = "intensity",
#' 	descrip = "test import", fileExt = ".jdx")
#' sumSpectra(spec)
#' plotSpectra(spec, lab.pos = 3.5, main = "Reduced Fat Potato Chip")
#' 

files2SpectraObject <- function(gr.crit = NULL,
	gr.cols = c("auto"),
	freq.unit = "no frequency unit provided",
	int.unit = "no intensity unit provided",
	descrip = "no description provided",
	fileExt = "\\.(csv|CSV)$",
	out.file = "mydata", debug = FALSE, ...) {
		
# Function to Read & Prep Spectroscopic Data
# from raw data files into an Spectra object
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Aug 2009

# This function acts on all files in the working directory with the specified extension

# Files should have freq in column 1, absorbance/intensity in column 2.
# There may or may not be a header (default is FALSE for read.table)

# DX files can be parsed, but are handled via readJDX

	if (!requireNamespace("R.utils", quietly = TRUE)) {
		stop("You need to install package R.utils to use this function")
		}

	message("The default behavior of this function has changed as of July 2016.\nSee ?files2SpectraObject.  Really: please read it!")
	
	if (is.null(gr.crit)) stop("No group criteria provided to encode data")
	
	DX = FALSE
	if (grepl("(dx|DX|jdx|JDX)", fileExt)) {
		DX <- TRUE
		if (!requireNamespace("readJDX", quietly = TRUE)) {
			stop("You need to install package readJDX to import JCAMP-DX files")
			}
		}
		
	# First set up some common stuff
	
	files <- list.files(pattern = fileExt)
	files.noext <- file_path_sans_ext(files)

	spectra <- list()
	spectra$names <- files.noext

	if (debug) message("\nfiles2SpectraObject is checking the first file")
	if (!DX) {
		temp <- read.table(files[1], ...)
		spectra$freq <- temp[,1]
		}
	if (DX) {
		temp <- readJDX::readJDX(file = files[1], debug = debug)
		spectra$freq <- temp[[2]]$x
		}
		
	if (class(spectra$freq) == "integer") {
		if (debug) message("\nConverting integer frequency values to numeric")
		spectra$freq <- as.numeric(spectra$freq)
		}
	
	spectra$data <- matrix(data = NA_real_, nrow = length(files), ncol = length(spectra$freq))
	
	# Loop over all files (you have to read the whole file then grab
	# just the part you want, i.e. the 2nd column)

	if (debug) message("\nfiles2SpectraObject will now import your files")
	
	for (i in 1:length(files)) {
		if (debug) cat("Importing file: ", files[i], "\n")
		if (!DX) {
			temp <- read.table(files[i], ...)
			spectra$data[i,] <- temp[,2]
			}
		if (DX) {
			temp <- readJDX::readJDX(files[i], debug = debug, ...)
			spectra$data[i,] <- temp[[2]]$y
			}
		}

	# Go get group assignments & colors, to complete assembly of spectra

	spectra <- groupNcolor(spectra, gr.crit, gr.cols)
	spectra$unit[1] <- freq.unit
	spectra$unit[2] <- int.unit
	spectra$desc <- descrip
	chkSpectra(spectra)
	
	datafile <- paste(out.file, ".RData", sep = "")

	R.utils::saveObject(spectra, file = datafile)
	
	return(spectra)
	}


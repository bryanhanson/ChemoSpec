#'
#' Import Data into a Spectra Object
#'
#' These functions import data into a \code{\link{Spectra}} object.  For "csv-like" files they use
#' \code{\link[utils]{read.table}}, so they are very flexible in regard to file formatting.
#' \pkg{Be sure to see the \ldots argument below for important details you need to provide.}
#' \code{files2SpectraObject} can also read JCAMP-DX files and will do so if \code{fileExt} is
#' any of \code{"dx"}, \code{"DX"}, \code{"jdx"} or \code{"JDX"}.
#'
#' @param gr.crit Group Criteria.  A vector of character strings which will be
#'        searched for among the file/sample names in order to assign an individual
#'        spectrum to group membership. This is done using grep, so characters
#'        like "." (period/dot) do not have their literal meaning (see below).
#'        Warnings are issued if there are file/sample names that don't match entries in
#'        \code{gr.crit} or there are entries in \code{gr.crit} that don't match any file names.
#'
#' @param gr.cols Group Colors.  See \code{\link{colorSymbol}} for some options. One of the following:
#'   \itemize{
#'     \item Legacy behavior and the default: The word \code{"auto"}, in which case up to 8 colors will
#'           be automatically assigned from package \code{RColorBrewer Set1}.
#'     \item \code{"Col7"}. A unique set of up to 7 colorblind-friendly colors is used.
#'     \item \code{"Col8"}. A unique set of up to 8 colors is used.
#'     \item \code{"Col12"}. A mostly paired set of up to 12 colors is used.
#'     \item A vector of acceptable color designations with the same length as \code{gr.crit}.
#'   }
#'       Colors will be assigned one for one, so the first element of
#'       \code{gr.crit} is assigned the first element of \code{gr.col} and so forth.  For \code{Col12}
#'       you should pay careful attention to the order of \code{gr.crit} in order to match up colors.
#'
#' @param freq.unit A character string giving the units of the x-axis
#'        (frequency or wavelength).
#'
#' @param int.unit A character string giving the units of the y-axis (some sort
#'        of intensity).
#'
#' @param descrip A character string describing the data set that will be stored.  This string is used
#'        in some plots so it is recommended that its length be less than about 40 characters.
#'
#' @param fileExt A character string giving the extension of the files to be
#'        processed. \code{regex} strings can be used.  For instance, the default
#'        finds files with either \code{".csv"} or \code{".CSV"} as the extension.
#'        Matching is done via a grep process, which is greedy.  See also the
#'        "Advanced Tricks" section.
#'
#' @param out.file A file name.  The completed object of S3 class \code{\link{Spectra}} will be written
#'        to this file.
#'
#' @param debug Logical. Applies to \code{files2SpectraObject} only.
#'        Set to \code{TRUE} for troubleshooting when an error
#'        is thrown during import.  In addition, values of 1-5 will work
#'        when importing a JCAMP-DX file via \code{fileExt = "\\.jdx"} etc.  These
#'        will be passed through to the \code{\link[readJDX]{readJDX}} function.
#'        See there for much more info on importing JCAMP-DX files.
#'
#' @param in.file Character.  Applies to \code{matrix2SpectraObject} only.
#'        Input file name, including extension.  Can be a vector of file names.
#'
#' @param chk Logical. Applies to \code{matrix2SpectraObject} only.
#'        Should the \code{Spectra} object be checked for integrity?  If you are having trouble
#'        importing your data, set this to \code{FALSE} and do \code{str(your object)} to troubleshoot.
#'
#' @param ...  Arguments to be passed to \code{\link[utils]{read.table}},
#'        \code{\link{list.files}} or \code{readJDX}; see the "Advanced Tricks" section.
#'        For \code{read.table}, \pkg{You MUST supply values for \code{sep}, \code{dec} and \code{header} consistent
#'        with your file structure, unless they are the same as the defaults for \code{\link[utils]{read.table}}}.
#'
#' @return A object of class \code{\link{Spectra}}.  An \emph{unnamed} object
#'         of S3 class \code{\link{Spectra}} is also written to \code{out.file}.  To
#'         read it back into the workspace, use \code{new.name <- loadObject(out.file)}
#'         (\code{loadObject} is package \pkg{R.utils}).
#'
#' @section files2SpectraObject:
#'
#' \code{files2SpectraObject} acts on all files in the current working
#' directory with the specified \code{fileExt} so there should be no
#' extra files of that type hanging around (except see next paragraph).
#' The first column should contain the frequency values and the second column the intensity values. The
#' files may have a header or not (supply \code{header = TRUE/FALSE} as
#' necessary).  The frequency column is assumed to be the same in all files.
#'
#' If \code{fileExt} contains any of \code{"dx"}, \code{"DX"}, \code{"jdx"} or
#' \code{"JDX"}, then the files will be processed by \code{\link[readJDX]{readJDX}}.
#' Consider setting \code{debug = TRUE}, or \code{debug = 1} etc for this format, as there are many
#' options for JCAMP, and many are untested. See \code{\link[readJDX]{readJDX}} for options and
#' known limitations.
#'
#' @section matrix2SpectraObject:
#'
#' This function takes one or more csv-like files, containing frequencies in the first
#' column, and samples in additional columns, and processes it into a
#' \code{\link{Spectra}} object.  The file MUST have a header row which includes
#' the sample names.  There need not be a header for the first (frequency)
#' column.  If more than one file given, they must all have the same frequency entries.
#'
#' @section gr.crit and Sample Name Gotchas:
#'
#' The matching of \code{gr.crit} against the sample file names
#' (in \code{files2SpectraObject}) or column headers/sample names
#' (in code{matrix2SpectraObject}) is done one at
#' a time, in order, using grep.  While powerful, this has the potential to lead
#' to some "gotchas" in certain cases, noted below.
#'
#' Your file system may allow file/sample names which \code{R} will not like, and will
#' cause confusing behavior.  File/sample names become variables in \code{ChemoSpec}, and \code{R}
#' does not like things like "-" (minus sign or hyphen) in file/sample names.  A hyphen
#' is converted to a period (".") if found, which is fine for a variable name.
#' However, a period in \code{gr.crit} is interpreted from the grep point of view,
#' namely a period matches any single character.  At this point, things may behave
#' very differently than one might hope.  See \code{\link{make.names}} for allowed
#' characters in \code{R} variables and make sure your file/sample names comply.
#'
#' The entries in \code{gr.crit} must be
#' mutually exclusive.  For example, if you have files with names like
#' "Control_1" and "Sample_1" and use \code{gr.crit = c("Control", "Sample")}
#' groups will be assigned as you would expect.  But, if you have file names
#' like "Control_1_Shade" and "Sample_1_Sun" you can't use \code{gr.crit =
#' c("Control",} \code{"Sample"}, \code{"Sun"}, \code{"Shade")} because each criteria is grepped in
#' order, and the "Sun/Shade" phrases, being last, will form the basis for your
#' groups.  Because this is a grep process, you can get around this by using
#' regular expressions in your \code{gr.crit} argument to specify the desired
#' groups in a mutually exclusive manner.  In this second example, you could
#' use \code{gr.crit = c("Control(.*)Sun"}, \code{"Control(.*)Shade"}, \code{"Sample(.*)Sun"},
#' \code{"Sample(.*)Shade")} to have your groups assigned based upon both phrases in
#' the file names.
#'
#' To summarize, \code{gr.crit} is used as a grep pattern, and the file/sample names
#' are the target.  Make sure your file/sample names comply with \code{\link{make.names}}.
#'
#' Finally, samples whose names are not matched using \code{gr.crit} are still
#' incorporated into the \code{\link{Spectra}} object, but they are not
#' assigned a group or color. Therefore they don't plot, but they do take up space in a
#' plot!  A warning is issued in these cases, since one wouldn't normally want
#' a spectrum to be orphaned this way.
#'
#' All these problems can generally be identified by running \code{\link{sumSpectra}}
#' once the data is imported.
#'
#' @section Advanced Tricks:
#'
#' The ... argument can be used to pass any argument to \code{read.table} or \code{list.files}.
#' This includes the possibility of passing arguments that will cause trouble later, for instance
#' \code{na.strings} in \code{read.table}.  While one might successfully read in data with \code{NA},
#' it will eventually cause problems.  The intent of this feature is to allow one to recurse
#' a directory tree containing the data, and/or to specify a starting point other than the current
#' working directory.  So for instance if the current working directory is not the directory containing
#' the data files, you can use \code{path = "my_path"} to point to the desired top-level
#' directory, and \code{recursive = TRUE} to work your way through a set of subdirectories.  In addition,
#' if you are reading in JCAMP-DX files, you can pass arguments to \code{readJDX} via ..., e.g. \code{SOFC = FALSE}.
#' Finally, while argument \code{fileExt} appears to be a file extension (from its
#' name and the description elsewhere), it's actually just a grep pattern that you can apply
#' to any part of the file name if you know how to construct the proper pattern.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/},
#'          as well as \code{\link{updateGroups}}.
#'
#' @keywords file
#' @keywords import
#'
#' @export files2SpectraObject matrix2SpectraObject
#'
#' @describeIn files2SpectraObject Import data from separate csv files
#'
#' @importFrom utils read.table setTxtProgressBar txtProgressBar
#' @importFrom tools file_path_sans_ext
#' @importFrom readJDX readJDX
#'
#' @examples
#' \dontrun{
#' # Grab an included file
#' ed <- system.file("extdata", package = "ChemoSpec")
#' tf <- "PCRF.jdx"
#' chk <- file.copy(
#'   from = file.path(ed, tf), to = file.path(getwd(), tf),
#'   overwrite = TRUE
#' )
#'
#' # Now read in the file, and plot
#' spec <- files2SpectraObject(
#'   gr.crit = "PCRF", freq.unit = "ppm", int.unit = "intensity",
#'   descrip = "test import", fileExt = "\\.jdx"
#' )
#' sumSpectra(spec)
#' plotSpectra(spec, lab.pos = 3.5, main = "Reduced Fat Potato Chip")
#' }
#'
files2SpectraObject <- function(gr.crit = NULL,
                                gr.cols = "auto",
                                freq.unit = "no frequency unit provided",
                                int.unit = "no intensity unit provided",
                                descrip = "no description provided",
                                fileExt = "\\.(csv|CSV)$",
                                out.file = "mydata", debug = FALSE, ...) {
  out <- tryCatch(
    {

      # This function acts on all files in the working directory with the specified extension

      # Files should have freq in column 1, absorbance/intensity in column 2.
      # There may or may not be a header (default is FALSE for read.table)

      # DX files can be parsed, but are handled via readJDX

      if (!requireNamespace("R.utils", quietly = TRUE)) {
        stop("You need to install package R.utils to use this function")
      }

      if (is.null(gr.crit)) stop("No group criteria provided to encode data")

      DX <- FALSE
      if (grepl("(dx|DX|jdx|JDX)", fileExt)) {
        DX <- TRUE
        if (!requireNamespace("readJDX", quietly = TRUE)) {
          stop("You need to install package readJDX to import JCAMP-DX files")
        }
      }

      # Clean up args found in ... for further use
      argsLF <- argsRT <- argsDX <- as.list(match.call())[-1] # THREE copies to be used momentarily
      argsRT <- .cleanArgs(argsRT, "read.table") # further update below
      argsLF <- .cleanArgs(argsLF, "list.files")
      argsLF <- c(argsLF, list(pattern = fileExt, full.names = TRUE))
      if (DX) argsDX <- .cleanArgs(argsDX, "readJDX")

      files <- do.call(list.files, argsLF)
      if (length(files) == 0L) stop("No files found. Is the extension wrong, or are we in the wrong directory?")
      files.noext <- tools::file_path_sans_ext(basename(files))

      spectra <- list()
      spectra$names <- files.noext

      if (debug) message("\nfiles2SpectraObject is checking the first file")
      if (!DX) {
        temp <- do.call(utils::read.table, args = c(argsRT, list(file = files[1])))
        spectra$freq <- temp[, 1]
      }
      if (DX) {
        temp <- do.call(readJDX::readJDX, args = c(argsDX, list(file = files[1], debug = debug)))
        spectra$freq <- temp[[4]]$x
      }

      if (inherits(spectra$freq, "integer")) {
        if (debug) message("\nConverting integer frequency values to numeric")
        spectra$freq <- as.numeric(spectra$freq)
      }

      spectra$data <- matrix(data = NA_real_, nrow = length(files), ncol = length(spectra$freq))

      # Loop over all files (you have to read the whole file then grab
      # just the part you want, i.e. the 2nd column)

      if (debug) message("\nfiles2SpectraObject will now import your files")

      if (!debug) {
        # Code for progress bar contributed by Reinhard Kerschner
        env <- environment() # NEW set environment for progress bar
        pb_Max <- length(files)
        counter <- 0
        message("\nReading ", pb_Max, " files...\n")
        pb <- txtProgressBar(min = 0, max = pb_Max, style = 3)
      }

      for (i in 1:length(files)) {
        if (debug) cat("Importing file: ", files[i], "\n")
        if (!DX) {
          temp <- do.call(utils::read.table, args = c(argsRT, list(file = files[i])))
          spectra$data[i, ] <- temp[, 2]
        }
        if (DX) {
          temp <- do.call(readJDX::readJDX, args = c(argsDX, list(file = files[i], debug = debug)))
          spectra$data[i, ] <- temp[[4]]$y
        }

        if (!debug) {
          curVal <- get("counter", envir = env)
          assign("counter", curVal + 1, envir = env)
          setTxtProgressBar(get("pb", envir = env), curVal + 1)
        }
      } # end of looping over files

      if (!debug) {
        close(pb)
        message("\nAssigning ", pb_Max, " spectra to ", length(gr.crit), " groups...\n")
      }

      # Go get group assignments & colors, to complete assembly of spectra

      spectra <- .groupNcolor(spectra, gr.crit, gr.cols, mode = "1D")
      spectra$unit[1] <- freq.unit
      spectra$unit[2] <- int.unit
      spectra$desc <- descrip
      chkSpectra(spectra)

      if (!debug) message("Success!\n")

      datafile <- paste(out.file, ".RData", sep = "")

      R.utils::saveObject(spectra, file = datafile)

      return(spectra)
    },
    error = function(cond) {
      errmess <- "There was a problem importing your files!\n\nAre you importing csv or similar files?\nIf you got a message such as 'undefined columns selected'?\nyou probably need to specify sep, header and dec values\nPlease read ?files2SpectraObject for details\n\nFor any trouble importing files set debug = TRUE\n"
      message("\nError message from R: ", cond$message, "\n")
      message(errmess)
      return(NA)
    }
  ) # end of tryCatch

  return(out)
}

\name{files2SpectraObject}
\Rdversion{1.1}
\alias{files2SpectraObject}
\title{
Merge Files in a Directory into a Spectra Object
}
\description{
This function will read all files of a given type in a directory, and use the file names to construct group membership and assign colors and symbols.  All the data is placed into an object of S3 class \code{\link{Spectra}}.  This function uses \code{\link[utils]{read.table}} to read files so it is very flexible.
}
\usage{
files2SpectraObject(gr.crit = NULL, gr.cols = c("auto"),
freq.unit = "no frequency unit provided",
int.unit = "no intensity unit provided",
descrip = "no description provided",
fileExt = "\\\\.(csv|CSV)$",
out.file = "mydata", debug = FALSE, ...)
}
\arguments{
  \item{gr.crit}{
Group Criteria.  A vector of character strings which will be searched for among the file names in order to assign an individual spectrum/sample to group membership. Warnings are issued if there are file names that don't match entries in \code{gr.crit} or there are entries in \code{gr.crit} that don't match any file names. See Details for some nuances.
}
  \item{gr.cols}{
Group Colors.  Either the word "auto", in which case colors will be automatically assigned, or a vector of acceptable color names with the same length as \code{gr.crit}. In the latter case, colors will be assigned one for one, so the first element of \code{gr.crit} is assigned the first element of \code{gr.col} and so forth.  See details below for some other issues to consider.
}
  \item{freq.unit}{
A character string giving the units of the x-axis (frequency or wavelength).
}
  \item{int.unit}{
A character string giving the units of the y-axis (some sort of intensity).
}
  \item{descrip}{
A character string describing the data set that will be stored.  This string is used in some plots so it is recommended that its length be less than about 40 characters.
}
  \item{fileExt}{
A character string giving the extension of the files to be processed. \code{regex} strings can be used.  For instance, the default finds files with either \code{".csv"} or \code{".CSV"} as the extension.  Matching is done via a grep process, which is greedy.
}
\item{out.file}{
A file name acceptable to the \code{save} function.  The completed object of S3 class \code{\link{Spectra}} will be written to this file.
}
  \item{debug}{
Logical; set to \code{TRUE} for troubleshooting when an error is thrown during import.
}
  \item{...}{
Arguments to be passed to \code{\link[utils]{read.table}}.  You MUST supply values for \code{sep}, \code{dec} and \code{header} consistent with your file structure, unless they are the same as the defaults for \code{\link[utils]{read.table}}.
}

}
\details{
The linking of groups with colors is handled by \code{\link{groupNcolor}}.


The matching of \code{gr.crit} against the sample file names is done one at a time, in order.  This means that the entries in  \code{gr.crit} must be mutually exclusive.  For example, if you have files with names like "Control_1" and "Sample_1" and use \code{gr.crit = c("Control", "Sample")} groups will be assigned as you would expect.  But, if you have file names like "Control_1_Shade" and "Sample_1_Sun" you can't use \code{gr.crit = c("Control", "Sample", "Sun", "Shade")} because each criteria is grepped in order, and the "Sun/Shade" phrases, being last, will form the basis for your groups.  Because this is a grep process, you can get around this by using regular expressions in your \code{gr.crit} argument to specify the desired groups in a mutually exclusive manner.  In this second example, you could use \code{gr.crit = c("Control(.*)Sun", "Control(.*)Shade", "Sample(.*)Sun", "Sample(.*)Shade")} to have your groups assigned based upon both phrases in the file names.


\code{files2SpectraObject} acts on all files in the current working directory with the specified \code{fileExt}.  The first column should contain the frequency values and the second column the intensity values. The files may have a header or not (supply \code{header = TRUE/FALSE} as necessary).  The frequency column is assumed to be the same in all files.


If \code{fileExt} contains any of \code{"dx"}, \code{"DX"}, code{"jdx"} or \code{"JDX"}, then the files will be processed by \code{\link{readJDX}}.  Consider setting \code{debug = TRUE} for this format, as there are many options for JCAMP, and most are untested. See \code{\link{readJDX}} for known limitations.


There should be no other files of the given extension in the directory except those containing the data to be processed by \code{files2SpectraObject}, as all files with that format in the directory will be processed.
}
\value{
A object of class \code{\link{Spectra}}.  An \emph{unnamed} object of S3 class \code{\link{Spectra}} is also written to \code{out.file}.  To read it back into the workspace, use \code{new.name <- loadObject(out.file)} (\code{loadObject} is package \code{R.utils}).
}

\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\seealso{\code{\link{matrix2SpectraObject}}.
}


\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\section{Warning }{
Files whose names are not matched using gr.crit are still incorporated into the \code{\link{Spectra}} object, but they are not assigned a group or color. They don't plot, but they do take up space in a plot!  A warning is issued in these cases, since one wouldn't normally want a spectrum to be orphaned this way.
}

\keyword{ file }

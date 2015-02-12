\name{files2SpectraObject}
\Rdversion{1.1}
\alias{files2SpectraObject}
\title{
Merge Files in a Directory into a Spectra Object
}
\description{
This function will read all files of a given type in a directory, and use the file names to construct group membership and assign colors and symbols.  All the data is placed into an object of S3 class \code{\link{Spectra}}.  This is the only way to create a \code{\link{Spectra}} object automatically.
}
\usage{
files2SpectraObject(gr.crit = NULL, gr.cols = c("auto"),
freq.unit = "no frequency unit provided",
int.unit = "no intensity unit provided",
descrip = "no description provided",
format = "csv",
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
  \item{format}{
A character string giving the format of the files to be processed.  Default is \code{csv} for US-style csv files.  Alternatively, you can specify \code{csv2} for EU-style csv files, or \code{dx} for JCAMP-DX files.
}

  \item{out.file}{
A file name acceptable to the \code{save} function.  The completed object of S3 class \code{\link{Spectra}} will be written to this file.
}
  \item{debug}{
Logical; set to \code{TRUE} for troubleshooting when using \code{format = "dx"}.
}
  \item{...}{
Other arguments to be passed downstream.
}

}
\details{
The linking of groups with colors is handled by \code{\link{groupNcolor}}.


The matching of \code{gr.crit} against the sample file names is done one at a time, in order.  This means that the entries in  \code{gr.crit} must be mutually exclusive.  For example, if you have files with names like "Control_1" and "Sample_1" and use \code{gr.crit = c("Control", "Sample")} groups will be assigned as you would expect.  But, if you have file names like "Control_1_Shade" and "Sample_1_Sun" you can't use \code{gr.crit = c("Control", "Sample", "Sun", "Shade")} because each criteria is grepped in order, and the "Sun/Shade" phrases, being last, will form the basis for your groups.  Because this is a grep process, you can get around this by using regular expressions in your \code{gr.crit} argument to specify the desired groups in a mutually exclusive manner.  In this second example, you could use \code{gr.crit = c("Control(.*)Sun", "Control(.*)Shade", "Sample(.*)Sun", "Sample(.*)Shade")} to have your groups assigned based upon both phrases in the file names.


\code{files2SpectraObject} acts on the files in the current working directory.  If \code{format = "csv"} these should be .csv files with the first column containing the frequency values and the second column containing the intensity values.  The columns should be unlabeled (i.e. no header row).  The frequency column is assumed to be the same in all .csv files.  If \code{format = "dx"}, then the corresponding file type will be processed (consider setting \code{debug = TRUE} for this format).  See \code{\link{readJDX}} for limitations (there are many options for JCAMP, and most are untested).


There should be no other files of the given format (extension) in the directory except those containing the data to be processed by \code{files2SpectraObject}, as all files with that format in the directory will be processed.
}
\value{
A object of class \code{\link{Spectra}}.  An \emph{unnamed} object of S3 class \code{\link{Spectra}} is also written to \code{out.file}.  To read it back into the workspace, use \code{new.name <- loadObject(out.file)}, found in package \code{R.utils}.
}

\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\section{Warning }{
Files whose names are not matched using gr.crit are still incorporated into the \code{\link{Spectra}} object, but they are not assigned a group or color and therefore don't plot, though they do take up space in a plot!
}

\keyword{ file }

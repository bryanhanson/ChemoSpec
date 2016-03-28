\name{matrix2SpectraObject}
\alias{matrix2SpectraObject}
\title{
Process a Data Matrix in a csv File into a Spectra Object
}
\description{
This function takes a csv-like file, containing frequencies in the first column, and samples in additional columns, and processes it into a \code{Spectra} object.  The file should contain a header row which includes the sample names.  The file can have any combination of delimiters and decimal marks (see below).
}
\usage{
matrix2SpectraObject(gr.crit = NULL,
gr.cols = c("auto"),
freq.unit = "no frequency unit provided",
int.unit = "no intensity unit provided",
descrip = "no description provided",
in.file = NULL,
out.file = "mydata",
chk = TRUE, ...)
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
A character string describing the data set that will be stored.  This string is used in some plots so it is recommended that its length be less than about 40 characters.}
  \item{in.file}{
Character.  Input file name, including extension.
}
  \item{out.file}{
A file name acceptable to the \code{save} function.  The completed object of S3 class \code{\link{Spectra}} will be written to this file.
}
  \item{chk}{
Logical.  Should the \code{Spectra} object be checked for integrity?  If you are having trouble importing your data, set this to \code{FALSE} and do \code{str(your object)} to troubleshoot.
}
  \item{\dots}{
Additional arguments to be passed to \code{read.table}.  In particular, you MUST give a value for \code{sep} and \code{decimal}, nothing is assumed.  This gives maximum flexibility in reading in a variety of formats.
}
}
\details{
The linking of groups with colors is handled by \code{\link{groupNcolor}}.


The matching of \code{gr.crit} against the sample file names is done one at a time, in order.  This means that the entries in  \code{gr.crit} must be mutually exclusive.  For example, if you have files with names like "Control_1" and "Sample_1" and use \code{gr.crit = c("Control", "Sample")} groups will be assigned as you would expect.  But, if you have file names like "Control_1_Shade" and "Sample_1_Sun" you can't use \code{gr.crit = c("Control", "Sample", "Sun", "Shade")} because each criteria is grepped in order, and the "Sun/Shade" phrases, being last, will form the basis for your groups.  Because this is a grep process, you can get around this by using regular expressions in your \code{gr.crit} argument to specify the desired groups in a mutually exclusive manner.  In this second example, you could use \code{gr.crit = c("Control(.*)Sun", "Control(.*)Shade", "Sample(.*)Sun", "Sample(.*)Shade")} to have your groups assigned based upon both phrases in the file names.

}
\value{
A object of class \code{\link{Spectra}}.  An \emph{unnamed} object of S3 class \code{\link{Spectra}} is also written to \code{out.file}.  To read it back into the workspace, use \code{new.name <- loadObject(out.file)} (\code{loadObject} is package \code{R.utils}).
}

\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\section{Warning }{
Samples whose names are not matched using gr.crit are still incorporated into the \code{\link{Spectra}} object, but they are not assigned a group or color and therefore don't plot, but they do take up space in a plot!
}

\keyword{ file }

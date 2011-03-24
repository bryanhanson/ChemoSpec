\name{getManyCsv}
\Rdversion{1.1}
\alias{getManyCsv}
\title{
Merge CSV Files in a Directory into a Spectra Object
}
\description{
This function will read all .csv files in a directory, and use the file names to construct group membership and assign colors and symbols.  All the data is placed into an object of S3 class \code{"Spectra"}.
}
\usage{
getManyCsv(gr.crit = NULL, gr.cols = c("auto"),
freq.unit = "no frequency unit provided",
int.unit = "no intensity unit provided",
descrip = "no description provided",
format = "csv", out.file = "mydata")
}
\arguments{
  \item{gr.crit}{
Group Criteria.  A vector of character strings which will be searched for among the file names in order to assign an individual spectrum/sample to group membership.
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
A character string describing the data set that will be stored.  This string is used in some plots so it is recommended that it's length be less than about 40 characters.
}
  \item{format}{
A character string giving the format of the .csv files to be processed.  Default is \code{csv} for US-style csv files.  Alternatively, you can specify \code{csv2} for EU-style csv files.
}
  \item{out.file}{
A file name acceptable to the \code{save} function.  The completed object of S3 class \code{"Spectra"} will be written to this file.
}
}
\details{
The linking of groups with colors is handled by \code{\link{groupNcolor}}.  \code{getManyCsv} acts on the files in the current working directory.  These should be .csv files with the first column containing the frequency values and the second column containing the intensity values.  The columns should be unlabeled.  The frequency column is assumed to be the same in all .csv files.  There should be no .csv files in the directory except those containing the data to be processed by \code{getManyCsv}, as all .csv files in the directory will be processed.
}
\value{
An \emph{unnamed} object of S3 class \code{\link{Spectra}} will be written to \code{out.file}.  To retrieve it, use \code{new.name <- loadObject(out.file)}.
}

\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\section{Warning }{
Files whose names are not matched using gr.crit are still incorporated into the \code{"Spectra"} object, but they are not assigned a group or color and therefore don't plot, though they do take up space in a plot!  I will fix this eventually.
}

\keyword{ file }

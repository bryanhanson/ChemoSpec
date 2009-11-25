\name{getManyCsv}
\Rdversion{1.1}
\alias{getManyCsv}
\title{
Merge CSV Files in a Directory into a Spectra Object
}
\description{
This function will read all .csv files in a directory, and use the file names to construct group membership and assign colors.  All the data is placed into an object of S3 class \code{"Spectra"}.
}
\usage{
getManyCsv(gr.crit = NULL, gr.cols = c("auto"),
	freq.unit = "no frequency unit provided", NMR = FALSE,
	int.unit = "no intensity unit provided",
	descrip = "no description provided",
	format = "original", out.file = "mydata")
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
  \item{NMR}{
A logical indicating if the type of spectra is traditionally plotted with higher numbers on the left (e.g. NMR or IR).
}
  \item{int.unit}{
A character string giving the units of the y-axis (some sort of intensity).
}
  \item{descrip}{
A character string describing the data set that will be stored.  This string is used in some plots so it is recommended that it's length be less than about 40 characters.
}
  \item{format}{
A character string giving the format of the .csv files to be processed.  Currently set to \code{"original"} but not used; intended for future flexibility.
}
  \item{out.file}{
A file name acceptable to the \code{save} function.  The completed object of S3 class \code{"Spectra"} will be written to this file.
}
}
\details{
The linking of groups with colors is handled by \code{\link{groupNcolor}}.  The user may specify any color name known to R.  However, if you plan to use \code{rggobi} and \code{GGobi} to view the data later, keep in mind that \code{ggobi} only uses certain color schemes (although there are many options).  In the case of \code{ChemoSpec}, two particular options have been hard-coded into the function \code{\link{plotScoresG}} for simplicity.  If you plan to use \code{plotScoresG}, you should use choose from one of the two color schemes now if you want all your graphics to use the same scheme.  Keep in mind that these colors must be used in order (though you can use the order of argument \code{gr.crit} to associate a particular group with a particular color:

primary scheme: \code{c("red3", "dodgerblue4", "forestgreen", "purple4",
	"orangered", "yellow", "orangered4", "violetred2")}

pastel scheme: \code{c("seagreen", "brown2", "skyblue2", "hotpink3",
	"chartreuse3", "darkgoldenrod2", "lightsalmon3", "gray48")}

If you want to see what these colors look like, use \code{display.brewer.pal(8, "Set1)} or \code{display.brewer.pal(8, "Set2)}. Finally, the difficulty to bear in mind here is that R plots are generally on a white background, so pale colors should be avoided, while \code{GGobi} plots on a black background, so dark colors should be avoided!
}
\value{
An object of S3 class \code{"Spectra"} and name "spectra" will be written to \code{out.file}.  You can change it's name later by loading it and assigning it to a new name.
}

\section{Calls}{\code{\link{groupNcolor}}}
\section{Called by}{Top level function, called by user.}

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

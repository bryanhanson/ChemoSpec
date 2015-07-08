\name{cleanSTOCSYpeaks}
\alias{cleanSTOCSYpeaks}
\title{
Process a List of Cross Peaks
}
\description{
This function assists the direct analysis of a STOCSY plot.  During manual inspection of the plot, one can record a driver peak and the corresponding cross peaks.  Since a STOCSCY plot is a contour plot, the exact position of a peak is subject to some interpretation.  Further, since a STOCSY plot is typically inspected in sectors, driver peaks may vary slightly across sectors.  This function will take the list made during inspection and clean it up, returning a data frame of the resulting peaks.
}
\usage{
cleanSTOCSYpeaks(spectra, file = NULL, rowThres = 0.01, colThres = 0.01,
collapseRows = TRUE, collapseCols = TRUE, verbose = TRUE)
}
\arguments{
  \item{spectra}{
An object of S3 class \code{\link{Spectra}}.
}
  \item{file}{
The name of a csv file containing the cross peak information.  The first column should be the driver peaks.  Other columns should give the frequencies of the cross peaks observed for a given driver (so for a given row, the first entry is the driver peak, and the rest of the entries are the cross peaks).  May have missing values.  Should have one value per cell.
}
  \item{rowThres}{
Numeric.  A value in the frequency domain.  Frequencies closer than this value will be collapsed.  See details.
}
  \item{colThres}{
Numeric.  A value in the frequency domain.  Frequencies closer than this value will be collapsed.  See details.
}
  \item{collapseRows}{
Logical.  Should the rows be collapsed if possible?
}
  \item{collapseCols}{
Logical.  Should the columns be collapsed if possible?
}
  \item{verbose}{
Logical.  Shall diagnostic information be printed at the console?
}
}
\details{
The larger the values of the thresholds, the more collapsing will occur.  Collapsing means rows or columns separated by less than the threshold will be averaged.  For instance, two driver peaks at 4.05 and 4.07 would be collapsed if \code{rowThres} were 0.02 but not if it were 0.01.  Since the ultimate goal is to get unique peaks corresponding to biomarkers via look up in a database, one would generally want to collapse peaks to a degree. See the warnings.
}
\value{
A data frame containing the driver peaks in the first column, and the aligned cross peaks in the other columns.  The dimensions of this data frame will depend on the options to collapse and the thresholds.
}
\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\section{Warning}{Collapsing peaks can be tricky and may lead to very undesirable results.  First, threshold values below the resolution of the data will not collapse anything.  Second, if you have a series of peaks that are closely spaced, for instance a messy multiplet that arises from the overlap of several peaks, collapsing the series will replace the series with one peak when there were several originally.  You should look carefully at your 1D spectra to see if this could be a problem.  I recommend some experimentation to see if collapsing is wise for your data set.}


\examples{
##---- None at this time !! ----
}
\keyword{ utility }

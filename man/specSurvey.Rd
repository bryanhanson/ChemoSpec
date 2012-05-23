\name{specSurvey}
\alias{specSurvey}
\title{Plot Measures of Central Tendency and Spread for a Spectra Object
}
\description{
This function computes and plots various measures of central tendency and spread for a \code{"Spectra"} object.  Several different measures/spreads are available.  The computation can be done by group or using the entire data set.
}
\usage{
specSurvey(spectra, method = c("sd", "sem", "sem95", "mad", "iqr"),
by.gr = TRUE, title = "no title provided", ...)
}
\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"} to be analyzed.
}
  \item{method}{
One of \code{c("sd", "sem", "sem95", "mad", "iqr")}.  \code{sd} plots the mean +/- the standard deviation, \code{sem} computes the mean +/- the standard error of the mean, \code{sem95} computes the mean +/- the standard error at the 95 percent confidence interval, \code{mad} computes the median +/- the median absolute deviation, and finally, \code{iqr} plots the median + the upper hinge and - the lower hinge.
}
  \item{by.gr}{
Logical, indicating if the analysis is to be done by group or not.
}
  \item{title}{
A character string for the plot title.
}
  \item{\dots}{
Additional parameters to be passed to the plotting routines.
}
}
\value{
None; side effect is a plot
}
\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
data(CuticleIR)
specSurvey(CuticleIR, method = "iqr", title = "Cuticle IR")
}
\keyword{ hplot }

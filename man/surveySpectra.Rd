\name{surveySpectra}
\alias{surveySpectra}
\alias{surveySpectra2}
\title{Plot Measures of Central Tendency and Spread for a Spectra Object
}
\description{
These functions compute and plot various measures of central tendency and spread for a \code{"Spectra"} object.  Several different measures/spreads are available.  These are useful as an overview of where a data set varies the most.
}
\usage{
surveySpectra(spectra, method = c("sd", "sem", "sem95", "mad", "iqr"),
by.gr = TRUE, ...)

surveySpectra2(spectra, method = c("sd", "sem", "sem95", "mad", "iqr"),
lab.pos = 0.9*max(spectra$freq), ...)
}
\arguments{
  \item{spectra}{
An object of S3 class \code{\link{Spectra}} to be analyzed.
}
  \item{method}{
Character.  One of \code{c("sd", "sem", "sem95", "mad", "iqr")}.
}
  \item{by.gr}{
Logical, indicating if the analysis is to be done by group or not.
}
  \item{lab.pos}{
Numeric, giving the frequency where the label should be draw for \code{surveySpectra2}.
}
  \item{\dots}{
Additional parameters to be passed to the plotting routines.
}
}
\value{
None; side effect is a plot
}
\details{
For \code{surveySpectra} the method choice works as follows:  \code{sd} plots the mean spectrum +/- the standard deviation, \code{sem} plots the mean spectrum +/- the standard error of the mean, \code{sem95} plots the mean spectrum +/- the standard error at the 95 percent confidence interval, \code{mad} plots the median spectrum +/- the median absolute deviation, and finally, \code{iqr} plots the median spectrum + the upper hinge and - the lower hinge.

For \code{surveySpectra2}, the spectra are mean centered and plotted.  Below that, the relative summary statistic is plotted, offset, but on the same scale.
}
\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
data(SrE.IR)
myt <- expression(bolditalic(Serenoa)~bolditalic(repens)~bold(Extract~IR~Spectra))
surveySpectra(SrE.IR, method = "iqr", main = myt)
surveySpectra2(SrE.IR, method = "iqr", main = myt)
}
\keyword{ hplot }

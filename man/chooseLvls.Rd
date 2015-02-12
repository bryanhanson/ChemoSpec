\name{chooseLvls}
\alias{chooseLvls}

\title{
Select Levels for Contour and Image Type Plots
}

\description{
Given a matrix, this function will assist in selecting levels for preparing contour and image type plots.  For instance, levels can be spaced evenly, logrithmically, or exponentially.
}

\usage{
chooseLvls(M, n = 10, mode = "even", lambda = 1, base = 2, showHist = FALSE, ...)
}

\arguments{
  \item{M}{
A numeric matrix.
}
  \item{n}{
For all methods except \code{ecdf}, an integer giving the number of levels desired.  For \code{ecdf}, \code{n} should be one or more values in the interval (0...1).  For instance, a value of 0.6 corresponds to a single level in which 60 percent of the matrix values are below, and 40 percent above.
}
  \item{mode}{
Character.  One of \code{c("even", "log", "exp", "ecdf", "posexp", "negexp", "poslog", "neglog")}. \code{"even"} will create evenly spaced levels.  \code{"log"} will create levels which are more closely spaced at the extremes, while \code{"exp"} does the opposite.  The pos- or neg- versions select just the positive or negative values.  \code{"ecdf"} computes levels at the requested quantiles of the matrix.
}
  \item{lambda}{
Numeric.  A non-zero exponent used with \code{method = "exp"} and relatives.
}
  \item{base}{
Integer.  The base used with \code{method = "log"} and relatives.
}
  \item{showHist}{
Logical.  Shall a histogram be drawn showing the location of the chosen levels?
}
  \item{\dots}{
Arguments to be passed downstream.
}
}

\value{
A numeric vector giving the levels.
}

\note{
This function is called by \code{\link{corSpectra}} and \code{\link{covSpectra}} if levels are not provided to those functions.
}

\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{

set.seed(9)
MM <- matrix(runif(100, -1, 1), nrow = 10) # test data
tsts <- c("even", "log", "exp", "ecdf", "poslog", "posexp")
for (i in 1:length(tsts)) {
	nl <- 10
	if(tsts[i] == "ecdf")  nl <- seq(0.1, 0.9, 0.1)
	levels <- chooseLvls(M = MM, n = nl, mode = tsts[i], showHist = TRUE)
	}

}

\keyword{ utilities }

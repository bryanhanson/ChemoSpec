\name{labelExtremes}
\Rdversion{1.1}

\alias{labelExtremes}

\title{
Label Extreme Values in a 2D Data Set
}
\description{
A utility function which plots the sample names next to the sample points.  The number of samples labeled can be specified by passing it from the calling function.  Never called by the user. Uses base graphics.
}
\usage{
labelExtremes(data, names, tol)
}
\arguments{
  \item{data}{
A matrix containing the x values of the points/samples in the first column, and the y values in the second.
}
  \item{names}{
A character vector of sample names.  Length must match the number of rows in \code{x}.
}
  \item{tol}{
A number describing the fraction of points to be labeled.  \code{tol = 1.0} labels all the points; \code{tol = 0.05} labels \emph{approximately} the most extreme 5 percent.  Note that this is simply based upon quantiles, assumes that both x and y are each normally distributed, and treats x and y separately.  Thus, this is not a formal treatment of outliers, just a means of labeling points.  Note too that while this function could deal with groups separately, the way it is called by \code{\link{plotScoresDecoration}} lumps all groups together.}  
}

\value{
None.  Annotates the plot with labels.
}

\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\keyword{ utilities }


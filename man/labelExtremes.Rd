\name{labelExtremes}
\Rdversion{1.1}

\alias{labelExtremes}

\title{
Label Extreme Values in a Data Set
}
\description{
A utility function which plots the sample names next to the sample points.  The number of samples labeled can be specified by passing it from the calling function.  Never called by the user. 
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
A number describing the fraction of points to be labeled.  \code{tol = 1.0} labels all the points; \code{tol = 0.05} labels the most extreme 5 percent.}
}

\value{
None.  Annotates the plot with labels.
}

\section{Called by}{\code{\link{plotScoresDecoration}}}

\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\keyword{ utilities }


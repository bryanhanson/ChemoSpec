\name{labelExtremes3d}
\alias{labelExtremes3d}
\title{
Identify Extreme Values in 3D
}
\description{
A utility function to identify the extreme values in a 3D plot data set, presumably so that they can be labeled.  Algorithm is similar to \code{\link{labelExtremes}}, except that \code{labelExtremes3d} does not do the plotting (because the results are used by functions that use different plotting paradigms).  An internal function, not generally called by the user.
}
\usage{
labelExtremes3d(data, names, tol)
}
\arguments{
  \item{data}{
A matrix of 3 columns containing x, y and z values for the labels, with rows corresponding to sample names.
}
  \item{names}{
A character vector of sample names; must have length equal to \code{nrow(data)}.
}
  \item{tol}{
A number describing the fraction of points to be labeled.  \code{tol = 1.0} labels all the points; \code{tol = 0.05} labels \emph{approximately} the most extreme 5 percent.  Note that this is simply based upon quantiles, assumes that x, y and z are each normally distributed, and treats x, y and yz separately.  Thus, this is not a formal treatment of outliers, just a means of labeling points.  Note too that while this function could deal with groups separately, the way it is called by \code{\link{plotScoresRGL}} lumps all groups together.
}  
}

\value{
A data frame containing the x, y and z coordinates, along with the corresponding labels.
}


\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\keyword{ utilities }

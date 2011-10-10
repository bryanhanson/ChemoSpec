\name{rowDist}
\alias{rowDist}
\title{ Compute Distance Between Rows of a Matrix }
\description{
This function is a wrapper to compute the distance between rows of a matrix using a number of methods.  Some of these are available in package \code{\link{stats}} and some in \code{amap}.  All this function does is determine which method is requested and then the distance calculation is done by the appropriate method.
}
\usage{
rowDist(x, method)
}
\arguments{
  \item{x}{
A matrix whose rows will be used for the distance calculation.
}
  \item{method}{
A character; one of \code{c("pearson", "correlation", "spearman", "kendall", "euclidean", "maximum", "manhattan", "canberra","binary", "minkowski")}.
}
}
\details{
Methods \code{c("euclidean", "maximum", "manhattan", "canberra","binary", "minkowski")} are sent to function \code{\link{dist}} in package \code{\link{stats}} while methods \code{c("pearson", "correlation", "spearman", "kendall")} are handled by \code{\link{Dist}} in package \code{amap}.  See the respective help pages for details.
}
\value{
An object of class \code{dist}.
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}  Suggested and original written by Roberto Canteri.
}
\keyword{ utilities }

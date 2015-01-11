\name{q2rPCA}
\Rdversion{1.1}
\alias{q2rPCA}
\alias{r2qPCA}

\title{
Conversion Between PCA Classes
}

\description{
Utility to convert objects of S3 class \code{"prcomp"} (Q-mode PCA) to objects of S3 class \code{"princomp"} (R-mode PCA) or \emph{vice-versa}.  Not likely to be called by most users.
}

\usage{
q2rPCA(x)
r2qPCA(x)
}
\arguments{
  \item{x}{
 An object of either class \code{"prcomp"} or class \code{"princomp"}.  It will be converted to a form that can be used by functions expecting either class.
}
}

\details{
In the conversion, the necessary list elements are added; the "old" elements are not deleted (and user added list elements are not affected).  To indicate this, the class attribute is updated to include class \code{"conPCA"}.  The new object can then be used by functions expecting either class prcomp or princomp.  For details of the structure of \code{\link{prcomp}} or \code{\link{princomp}}, see their respective help pages.
}

\value{
A list of class \code{"conPCA"}.  Note that the order of the elements will vary depending upon the direction of conversion.
\item{loadings}{ The loadings from \code{"princomp"}, or a copy of the rotations from \code{"prcomp"}.}
\item{scores}{ The scores from \code{"princomp"}, or a copy of the x values from \code{"prcomp"}.}
\item{call}{ The call.  Objects of class \code{"prcomp"} do not store the original call, so a place holder is used. Otherwise the unchanged call from \code{"princomp"}.}
\item{n.obs}{ The number of observations from \code{"princomp"}, or computed from the 1st dimension of x in \code{"prcomp"}.}
\item{class}{ \code{"conPCA"} is pre-pended to the existing class.}
\item{sdev}{ Unchanged from original.}
\item{center}{ Unchanged from original.}
\item{scale}{ Unchanged from original.}
}

\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
\code{\link{prcomp}}, \code{\link{princomp}}
}

\keyword{classes}
\keyword{utilities}
